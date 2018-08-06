;;; comb-common.el --- Common utilities -*- lexical-binding: t -*-

;;; Code:

(require 'comb-session)

(defface comb-match
  '((t :inherit isearch))
  "Face used to highlight the matches."
  :group 'comb)

(defvar comb--window-configuration nil
  "Window configuration snapshot.")

(defun comb--format-status (status)
  "Format result STATUS."
  (case status
    ('nil (propertize "UNDECIDED" 'face 'warning))
    ('approved (propertize "APPROVED" 'face 'success))
    ('rejected (propertize "REJECTED" 'face 'error))))

(defun comb--format-notes (notes)
  "Format result NOTES."
  (propertize notes 'face 'font-lock-comment-face))

(defun comb--format-file-location (path line)
  "Format file location (PATH and LINE)."
  (format "%s%s:%s"
          (file-name-directory path)
          (propertize (file-name-nondirectory path) 'face 'bold)
          (propertize (number-to-string line) 'face 'shadow)))

(defun comb--save-window-configuration ()
  "Save current window configuration if needed."
  (unless comb--window-configuration
    (setq comb--window-configuration (current-window-configuration))))

(defun comb--restore-window-configuration ()
  "Restore the saved window configuration, if any."
  (when comb--window-configuration
    (set-window-configuration comb--window-configuration)
    (setq comb--window-configuration nil)))

(defmacro comb--with-temp-buffer (name on-prepare on-exit keymap &rest body)
  "Create a disposable buffer named NAME.

The form ON-PREPARE is executed after the buffer creation.

The form ON-EXIT is executed when the user presses 'q'.

If KEYMAP is not nil the use it as a parent keymap.

BODY is executed in the context of the newly created buffer."
  ;; show a fresh new buffer
  `(let ((name ,name))
     (ignore-errors (kill-buffer name))
     (with-current-buffer (switch-to-buffer name)
       ,on-prepare
       ;; setup keymap
       (let ((keymap (make-sparse-keymap)))
         (set-keymap-parent keymap ,keymap)
         (define-key keymap (kbd "q")
           (lambda () (interactive) ,on-exit))
         (use-local-map keymap))
       ;; user body
       (let ((inhibit-read-only t)) ,@body))))

(defmacro comb--with-ignore-quit (&rest body)
  "Execute BODY catching the quit error and returning nil in case."
  `(condition-case nil ,@body (quit)))

(defun comb--valid-cursor-p (&optional cursor)
  "Return non-nil if the cursor (or CURSOR) is valid."
  (let ((cursor (or cursor (comb--cursor))))
    (and (>= cursor 0) (< cursor (length (comb--results))))))

(defun comb--get-result (&optional cursor)
  "Get the result under the cursor (or CURSOR)."
  (aref (comb--results) (or cursor (comb--cursor))))

(defun comb--get-info (&optional result)
  "Obtain the information associated to the current result (or RESULT)."
  (gethash (or result (comb--get-result)) (comb--infos)))

(defmacro comb--with-info (info &rest body)
  "Utility to modify the information associated access the current result.

INFO is the name of the information variable used in BODY."
  `(condition-case nil
       (let (result ,info)
         ;; get result
         (setq result (comb--get-result))
         ;; get associated info
         (setq ,info (gethash result (comb--infos) (cons nil nil)))
         ;; execute the user-provided body using info
         ,@body
         ;; update only if needed
         (unless (equal ,info (cons nil nil))
           (puthash result ,info (comb--infos))))
     (args-out-of-range nil)))

(provide 'comb-common)

;;; comb-common.el ends here
