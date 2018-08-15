;;; comb-browse.el --- Interactive menu -*- lexical-binding: t -*-

;;; Code:

(require 'comb-common)
(require 'comb-configure)
(require 'comb-filter)
(require 'comb-report)
(require 'comb-session)

(require 'cl-macs)
(require 'pulse)
(require 'seq)
(require 'subr-x)

(defconst comb--buffer-name "*Comb*"
  "Comb main buffer name.")

(defconst comb--default-keybindings
  '(("h" . comb-help)
    ("c" . comb-configure)
    ("p" . comb-prev)
    ("n" . comb-next)
    ("<" . comb-begin)
    (">" . comb-end)
    ("a" . comb-approve)
    ("r" . comb-reject)
    ("u" . comb-undecide)
    ("A" . comb-approve-next)
    ("R" . comb-reject-next)
    ("U" . comb-undecide-next)
    ("f" . comb-cycle-status-filter)
    ("F" . comb-set-notes-filter)
    ("!" . comb-annotate)
    ("t" . comb-report)
    ("w" . comb-new)
    ("l" . comb-load)
    ("s" . comb-save)
    ("v" . comb-visit)
    ("q" . comb-quit))
  "Default keybindings.")

(defconst comb-keymap
  ;; build a keymap from the default keybindings
  (let ((keymap (make-sparse-keymap)))
    (dolist (keybinding comb--default-keybindings keymap)
      (define-key keymap (car keybinding) (cdr keybinding))))
  "Comb keymap.")

;; TODO this needs proper refactoring...
(defun comb--display (&optional no-switch)
  "Prepare the Comb buffer for the current result.

And switch to it unless NO-SWITCH."
  (let (progress buffer result info relative-path path begin end)
    ;; creates a session silently the first time
    (unless comb--session
      (setq comb--session (make-comb--session)))
    ;; set up the comb buffer
    (with-current-buffer (get-buffer-create comb--buffer-name)
      (fundamental-mode)
      (suppress-keymap comb-keymap) ; just once is enough
      (use-local-map comb-keymap)
      (read-only-mode 1)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)
      ;; obtain cursor information
      (setq progress (comb--count-results))
      ;; when a proper result has to be displayed
      (when (comb--valid-cursor-p)
        ;; prepare variables
        (setq result (comb--get-result))
        (setq info (comb--get-info result))
        (setq relative-path (car result))
        (setq path (concat (file-name-as-directory (comb--root)) relative-path))
        (setq begin (cadr result))
        (setq end (cddr result))
        ;; TODO only if different? otherwise just goto result
        (when (file-readable-p path)
          ;; visit the file omitting warnings (e.g., same file)
          (setq buffer (find-file-noselect path t))
          ;; replace the content of the buffer
          (let ((inhibit-read-only t))
            (insert-buffer-substring buffer)
            (set-buffer-modified-p nil))
          ;; copy the major mode from the visited buffer and reset the keymap
          (funcall (with-current-buffer buffer major-mode))
          (use-local-map comb-keymap)
          ;; highlight the match permanently
          (overlay-put (make-overlay begin end) 'face 'comb-match)))
      ;; build the info line
      (setq
       header-line-format
       (list
        " "
        ;; show the current filter
        (comb--header-line-button
         'comb-cycle-status-filter
         (cl-case (comb--status-filter)
           ('t (propertize "All" 'face 'bold))
           ('nil (propertize "Undecided" 'face 'comb-undecided))
           ('approved (propertize "Approved" 'face 'comb-approved))
           ('rejected (propertize "Rejected" 'face 'comb-rejected))))
        (let ((filter (comb--notes-filter)))
          (unless (string-empty-p filter)
            (format " ~ %s"
                    (comb--header-line-button
                     'comb-set-notes-filter
                     (propertize filter 'face 'bold)))))
        " " (propertize "|" 'face 'shadow) " "
        ;; show result number and count
        (if (seq-empty-p (comb--results))
            (format "Configure (%s) or load (%s) a session %s Show help (%s)"
                    (comb--header-line-button 'comb-configure)
                    (comb--header-line-button 'comb-load)
                    (propertize "|" 'face 'shadow)
                    (comb--header-line-button 'comb-help))
          (concat
           (format "%s/%s"
                   ;; result index, show '?' when filters are not matched
                   (cond ((< (comb--cursor) 0) "^")
                         ((>= (comb--cursor) (length (comb--results))) "$")
                         (t (if (car progress) (1+ (car progress)) "?")))
                   ;; results matching the filters
                   (cdr progress))
           ;; also show the total number if different
           (when (/= (cdr progress) (length (comb--results)))
             (format " (%s)" (length (comb--results))))))
        ;; show result information
        (if (comb--valid-cursor-p)
            (concat
             " " (propertize "|" 'face 'shadow) " "
             ;; status flag
             (comb--format-status (car info))
             " "
             ;; result location
             (if buffer
                 (comb--header-line-button
                  'comb-visit (comb--format-file-location relative-path))
               ;; file not found
               (propertize relative-path 'face 'error))
             ;; notes
             (when (cdr info)
               (format " %s %s"
                       (propertize "|" 'face 'shadow)
                       (comb--header-line-button
                        'comb-annotate (comb--format-notes (cdr info))))))
          ;; show navigation help
          (unless (seq-empty-p (comb--results))
            (format " %s %s %s Show help (%s)"
                    (propertize "|" 'face 'shadow)
                    (if (< (comb--cursor) 0)
                        (format "Move to the next (%s)"
                         (comb--header-line-button 'comb-next))
                      (format "Move to the previous (%s)"
                       (comb--header-line-button 'comb-prev)))
                    (propertize "|" 'face 'shadow)
                    (comb--header-line-button 'comb-help))))))
      ;; redisplay the info line
      (force-mode-line-update)
      ;; switch to the comb buffer if requested (no-switch is only used to
      ;; invalidate the buffer when there are no results after a search)
      (unless no-switch
        (switch-to-buffer (current-buffer))
        (when (and (comb--valid-cursor-p) (file-readable-p path))
          ;; go to location
          (goto-char begin)
          ;; center horizontally only when long lines are truncated
          (when truncate-lines
            (set-window-hscroll
             (selected-window) (- (current-column) (/ (window-width) 2))))
          ;; center vertically the match if it fits the window, otherwise show
          ;; the most of it starting from the beginning
          (let (extent)
            (setq extent (or (ignore-errors (count-screen-lines begin end)) 1))
            (if (> extent (window-height))
                (recenter 0)
              (save-excursion
                (vertical-motion (/ extent 2))
                (recenter)))))))))

(defun comb-prev ()
  "Show the previous result."
  (interactive)
  (when comb--session
    (comb--seek -1)
    (comb--display)))

(defun comb-next ()
  "Show the next result."
  (interactive)
  (when comb--session
    (comb--seek +1)
    (comb--display)))

(defun comb-begin ()
  "Go before the first result."
  (interactive)
  (when comb--session
    (setf (comb--cursor) -1)
    (comb--display)))

(defun comb-end ()
  "Go after the last result."
  (interactive)
  (when comb--session
    (setf (comb--cursor) (length (comb--results)))
    (comb--display)))

(defun comb-approve ()
  "Mark the current result as approved."
  (interactive)
  (when comb--session
    (comb--with-info info (setcar info 'approved))
    (comb--display)))

(defun comb-reject ()
  "Mark the current result as rejected."
  (interactive)
  (when comb--session
    (comb--with-info info (setcar info 'rejected))
    (comb--display)))

(defun comb-undecide ()
  "Mark the current result as undecided."
  (interactive)
  (when comb--session
    (comb--with-info info (setcar info nil))
    (comb--display)))

(defun comb-approve-next ()
  "Mark the current result as approved and go to the next."
  (interactive)
  (when comb--session
    (comb--with-info info (setcar info 'approved))
    (comb--seek +1)
    (comb--display)))

(defun comb-reject-next ()
  "Mark the current result as rejected and go to the next."
  (interactive)
  (when comb--session
    (comb--with-info info (setcar info 'rejected))
    (comb--seek +1)
    (comb--display)))

(defun comb-undecide-next ()
  "Mark the current result as undecided and go to the next."
  (interactive)
  (when comb--session
    (comb--with-info info (setcar info nil))
    (comb--seek +1)
    (comb--display)))

(defun comb-cycle-status-filter ()
  "Cycle among all the possible status filters."
  (interactive)
  (when comb--session
    (let (values)
      (setq values '(nil approved rejected t)) ; XXX nil must be the first value
      (setf (comb--status-filter) (cadr (member (comb--status-filter) values))))
    (comb--display)))

(defun comb-set-notes-filter ()
  "Set the notes filter."
  (interactive)
  (when comb--session
    (setf (comb--notes-filter)
          (read-string "Notes filter regexp: " (comb--notes-filter)))
    (comb--display)))

(defun comb-annotate ()
  "Annotate the current result."
  (interactive)
  (when comb--session
    (let (notes)
      (comb--with-info
       info
       (setq notes (read-string "Notes: " (cdr info)))
       (setcdr info (if (string-blank-p notes) nil (string-trim notes))))
      (comb--display))))

(defun comb-configure ()
  "Show the configuration buffer."
  (interactive)
  (when comb--session
    (comb--configure)))

(defun comb-report ()
  "Show the report buffer."
  (interactive)
  (when comb--session
    (comb--report)))

(defun comb-new ()
  "Create a new session using the current directory as root."
  (interactive)
  (comb--session-new)
  (comb--display))

(defun comb-load ()
  "Load the session from file."
  (interactive)
  (comb--session-load)
  (comb--display))

(defun comb-save ()
  "Save the session to file."
  (interactive)
  (when comb--session
    (comb--session-save)
    (comb--display)))

(defun comb-visit ()
  "Visit the buffer containing the current result."
  (interactive)
  (when comb--session
    (let (result path begin)
      (when (comb--valid-cursor-p)
        (setq result (comb--get-result))
        (setq path (concat (file-name-as-directory (comb--root)) (car result)))
        (setq begin (cadr result))
        (find-file path)
        (goto-char begin)))))

(defun comb-help ()
  "Show this help buffer."
  (interactive)
  (comb--with-temp-buffer-window
   "*Comb: help*"
   ;; on quit
   (kill-buffer)
   ;; keymap
   nil
   ;; setup
   (setq buffer-read-only t)
   (visual-line-mode)
   ;; format the help buffer
   (let (max-length)
     ;; find the key sequence maximum length
     (setq
      max-length
      (apply #'max (mapcar
                    (lambda (keybinding)
                      (length (comb--keybinding-for (cdr keybinding))))
                    comb--default-keybindings)))
     ;; format the help buffer
     (insert
      (mapconcat
       (lambda (keybinding)
         (propertize
          (format (format "%%%ss  %%s" max-length)
                  (comb--keybinding-for (cdr keybinding))
                  (documentation (cdr keybinding)))
          ;; nicely wrap long lines
          'wrap-prefix (make-string (+ max-length 2) ?\s)))
       comb--default-keybindings "\n"))
     (insert "\n\nPress q to exit..."))))

(defun comb-quit ()
  "Suspend the comb session and kill the buffer."
  (interactive)
  (when comb--session
    (kill-buffer)
    (comb--restore-window-configuration)))

(defun comb--seek (skip)
  "Move the cursor to the next result according to SKIP."
  (let (first-time cursor)
    (setq first-time t)
    (setq cursor (comb--cursor))
    (if (seq-empty-p (comb--results))
        ;; no results at all
        (setf (comb--cursor) -1)
      ;; move the cursor to the next/previous valid result
      (while (or first-time
                 (and (comb--valid-cursor-p cursor)
                      (not (funcall #'comb--filter
                                    (comb--get-result cursor)))))
        (setf cursor (+ cursor (/ skip (abs skip))))
        (setq first-time nil))
      ;; limit the cursor to, at most, one before/after the valid range
      (setf (comb--cursor) (max (min cursor (length (comb--results))) -1)))))

(defun comb--keybinding-for (command)
  "Return the description of the first key bound to COMMAND."
  (let (key)
    (setq key (car (where-is-internal command comb-keymap)))
    (if key (key-description key) "UNBOUND")))

(defun comb--header-line-button (action &optional text)
  "Create a header button for ACTION using TEXT or its bound key."
  (let (keymap)
    (setq keymap (make-sparse-keymap))
    (define-key keymap [header-line mouse-1] action)
    (propertize
     (or text (propertize (comb--keybinding-for action) 'face 'bold))
     'mouse-face 'mode-line-highlight 'local-map keymap)))

(provide 'comb-browse)

;;; comb-browse.el ends here
