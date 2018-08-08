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

;; computed later from `comb--menu-default-keybindings'
(defvar comb--menu-keybindings)

;; keybindings used literally
(defvar comb-menu-configure-key)
(defvar comb-menu-help-key)
(defvar comb-menu-quit-key)

(defconst comb--menu-default-keybindings
  '(("h" . menu-help)
    ("c" . menu-configure)
    ("p" . menu-prev)
    ("n" . menu-next)
    ("<" . menu-begin)
    (">" . menu-end)
    ("a" . menu-approve)
    ("r" . menu-reject)
    ("u" . menu-undecide)
    ("A" . menu-approve-next)
    ("R" . menu-reject-next)
    ("U" . menu-undecide-next)
    ("f" . menu-cycle-status-filter)
    ("F" . menu-set-notes-filter)
    ("!" . menu-annotate)
    ("t" . menu-report)
    ("w" . menu-new)
    ("l" . menu-load)
    ("s" . menu-save)
    ("z" . menu-suspend)
    ("q" . menu-quit))
  "Default menu keybindings.")

(defun comb--browse ()
  "Start the interactive mode."
  ;; creates a session silently the first time
  (unless comb--session
    (setq comb--session (make-comb--session)))
  (comb--save-window-configuration)
  (pulse-momentary-unhighlight)
  (comb--start-menu))

(defun comb--start-menu ()
  "Show the interactive menu."
  (while
      (progn
        ;; always show the dashboard after a user choice
        (comb--show-dashboard)
        ;; handle user choice
        (funcall
         (or (assoc-default
              (key-description (read-key-sequence nil)) comb--menu-keybindings
              (lambda (elem key)
                ;; compare the value from custom variable
                (equal (symbol-value elem) key)))
             (lambda () (message "Invalid key") (comb--wait) t))))))

(defun comb--show-dashboard ()
  "Show information about the current result."
  (let (progress buffer result info path begin end)
    ;; reset highlight if any
    (pulse-momentary-unhighlight)
    ;; obtain ursor information
    (setq progress (comb--count-results))
    ;; when a proper result has to be displayed
    (when (comb--valid-cursor-p)
      ;; prepare variables
      (setq result (comb--get-result))
      (setq info (comb--get-info result))
      (setq path (concat (file-name-as-directory (comb--root)) (car result)))
      (setq begin (cadr result))
      (setq end (cddr result))
      ;; load the buffer containing the result
      (with-current-buffer
          ;; visit the file omitting warnings (e.g., same file)
          (setq buffer (switch-to-buffer (find-file-noselect path t)))
        ;; set the read-only mode as editing would mess the search results
        (read-only-mode 1)
        ;; go to location
        (goto-char begin)
        ;; highlight the match and require a key press to dismiss the pulse
        (let ((pulse-flag nil))
          (pulse-momentary-highlight-region begin end 'comb-match))))
    ;; print dasboard information without cluttering *Messages*
    (let ((message-log-max nil))
      (message
       (concat
        (if (seq-empty-p (comb--results))
            (format "Configure (%s) the session"
                    comb-menu-configure-key)
          (concat
           ;; result index and count
           (format "%s/%s"
                   ;; result index, show '?' when filters are not matched
                   (cond (buffer (if (car progress) (1+ (car progress)) "?"))
                         ((< (comb--cursor) 0) "^")
                         ((>= (comb--cursor) (length (comb--results))) "$"))
                   ;; results matching the filters
                   (cdr progress))
           ;; also show the total number if different
           (when (/= (cdr progress) (length (comb--results)))
             (format " (%s)" (length (comb--results))))))
        " "
        ;; show the current filter
        (propertize "|" 'face 'shadow)
        " Showing "
        (cl-case (comb--status-filter)
          ('t (propertize "all" 'face 'bold))
          ('nil (propertize "undecided" 'face 'comb-undecided))
          ('approved (propertize "approved" 'face 'comb-approved))
          ('rejected (propertize "rejected" 'face 'comb-rejected)))
        (let ((filter (comb--notes-filter)))
          (unless (string-empty-p filter)
            (format " matching %s" (propertize filter 'face 'bold))))
        " "
        (propertize "|" 'face 'shadow)
        (format " Help (%s) " comb-menu-help-key)
        (propertize "|" 'face 'shadow)
        (format " Quit (%s)" comb-menu-quit-key)
        ;; result information
        (when buffer
          (concat
           "\n"
           ;; status flag
           (comb--format-status (car info))
           " "
           ;; result location
           (comb--format-file-location
            path (with-current-buffer buffer (line-number-at-pos)))
           ;; notes
           (when (cdr info)
             (format "\n%s" (comb--format-notes (cdr info)))))))))
    ;; recenter after displaying the dashboard
    (when buffer
      (with-current-buffer buffer
        (recenter)
        (when truncate-lines
          (set-window-hscroll
           (selected-window) (- (current-column) (/ (window-width) 2))))))))

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

;; menu commands

(defun comb--menu-prev ()
  "Show the previous result."
  (comb--seek -1)
  t)

(defun comb--menu-next ()
  "Show the next result."
  (comb--seek +1)
  t)

(defun comb--menu-begin ()
  "Go to the beginning."
  (setf (comb--cursor) -1)
  t)

(defun comb--menu-end ()
  "Go to the end."
  (setf (comb--cursor) (length (comb--results)))
  t)

(defun comb--menu-approve ()
  "Mark the current result as approved."
  (comb--with-info info (setcar info 'approved))
  t)

(defun comb--menu-reject ()
  "Mark the current result as rejected."
  (comb--with-info info (setcar info 'rejected))
  t)

(defun comb--menu-undecide ()
  "Mark the current result as undecided."
  (comb--with-info info (setcar info nil))
  t)

(defun comb--menu-approve-next ()
  "Mark the current result as approved and go to the next."
  (comb--menu-approve)
  (comb--menu-next)
  t)

(defun comb--menu-reject-next ()
  "Mark the current result as rejected and go to the next."
  (comb--menu-reject)
  (comb--menu-next)
  t)

(defun comb--menu-undecide-next ()
  "Mark the current result as undecided and go to the next."
  (comb--menu-undecide)
  (comb--menu-next)
  t)

(defun comb--menu-cycle-status-filter ()
  "Cycle among all the possible status filters."
  (let (values)
    (setq values '(nil approved rejected t)) ; XXX nil must be the first value
    (setf (comb--status-filter) (cadr (member (comb--status-filter) values))))
  t)

(defun comb--menu-set-notes-filter ()
  "Set the notes filter."
  (comb--with-ignore-quit
   (setf (comb--notes-filter)
         (read-string "Notes filter: " (comb--notes-filter))))
  t)

(defun comb--menu-annotate ()
  "Annotate the current result."
  (let (notes)
    (comb--with-ignore-quit
     (comb--with-info
      info
      (setq notes (read-string "Notes: " (cdr info)))
      (setcdr info (if (string-blank-p notes) nil (string-trim notes))))))
  t)

(defun comb--menu-configure ()
  "Open the configuration buffer."
  (comb--configure)
  nil)

(defun comb--menu-report ()
  "Open the report buffer."
  (comb--report)
  nil)

(defun comb--menu-new ()
  "Create a new session."
  (comb--session-new)
  (comb--wait)
  t)

(defun comb--menu-load ()
  "Load the session from file."
  (comb--session-load)
  (comb--wait)
  t)

(defun comb--menu-save ()
  "Save the session."
  (comb--session-save)
  (comb--wait)
  t)

(defun comb--menu-suspend ()
  "Suspend the interactive browsing retaining the current window configuration."
  nil)

(defun comb--menu-help ()
  "Show this buffer."
  (comb--with-temp-buffer-window
   "*comb: help*"
   ;; on quit
   (progn (kill-buffer) (comb--browse))
   ;; keymap
   button-buffer-map
   ;; setup
   (setq buffer-read-only t)
   ;; format the help buffer
   (let (max-length)
     ;; find the key sequence maximum length
     (setq
      max-length
      (apply #'max (mapcar
                    (lambda (keybinding)
                      (length (symbol-value (car keybinding))))
                    comb--menu-keybindings)))
     ;; format the help buffer
     (insert
      (mapconcat
       (lambda (keybinding)
         (format (format "%%%ss  %%s" max-length)
                 (symbol-value (car keybinding))
                 (documentation (cdr keybinding))))
       comb--menu-keybindings "\n")
      "\n\nYou can ")
     (insert-text-button
      "customize" 'action (lambda (_) (customize-group 'comb)))
     (insert " these keybindings.\n\nPress q to exit...")))
  nil)

(defun comb--menu-quit ()
  "Quit the interactive browsing restoring the window configuration."
  (comb--restore-window-configuration)
  nil)

;; one custom variable is created for each element of the
;; `comb--menu-default-keybindings' alist
(setq comb--menu-keybindings
      (mapcar
       (lambda (keybinding)
         (let (key action name)
           (setq key (car keybinding))
           (setq action (intern (format "comb--%s" (cdr keybinding))))
           (setq name (intern (format "comb-%s-key" (cdr keybinding))))
           (cons (eval `(defcustom ,name ,key ,(documentation action)
                          :type 'string :group 'comb))
                 action)))
       comb--menu-default-keybindings))

(provide 'comb-browse)

;;; comb-browse.el ends here
