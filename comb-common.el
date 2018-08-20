;;; comb-common.el --- Common utilities -*- lexical-binding: t -*-

;;; Code:

(defface comb-match '((t :inherit match))
  "Face used to highlight the matches."
  :group 'comb)

(defface comb-undecided '((t :inherit shadow))
  "Face used to mark undecided results."
  :group 'comb)

(defface comb-approved '((t :inherit success))
  "Face used to mark approved results."
  :group 'comb)

(defface comb-rejected '((t :inherit error))
  "Face used to mark rejected results."
  :group 'comb)

(defface comb-notes '((t :inherit font-lock-comment-face))
  "Face used to display the notes."
  :group 'comb)

(defvar comb--window-configuration nil
  "Window configuration snapshot.")

(defun comb--format-status (status)
  "Format result STATUS."
  (cl-case status
    ('nil (propertize "UNDECIDED" 'face 'comb-undecided))
    ('approved (propertize "APPROVED" 'face 'comb-approved))
    ('rejected (propertize "REJECTED" 'face 'comb-rejected))))

(defun comb--format-notes (notes)
  "Format result NOTES."
  (propertize notes 'face 'comb-notes))

(defun comb--format-file-location (path &optional line)
  "Format file location (PATH and LINE)."
  (format "%s%s%s"
          (or (file-name-directory path) "")
          (propertize (file-name-nondirectory path) 'face 'bold)
          (if line (propertize (format ":%s" line) 'face 'shadow) "")))

(defun comb--save-window-configuration ()
  "Save current window configuration if needed."
  (unless comb--window-configuration
    (setq comb--window-configuration (current-window-configuration))))

(defun comb--restore-window-configuration ()
  "Restore the saved window configuration, if any."
  (when comb--window-configuration
    (set-window-configuration comb--window-configuration)
    (setq comb--window-configuration nil)))

(defmacro comb--with-temp-buffer-window (name on-exit keymap &rest body)
  "Create a disposable buffer named NAME.

The ON-EXIT form is executed when the user presses 'q'.

If KEYMAP is not nil the use it as a parent keymap.

BODY is executed in the context of the newly created buffer."
  ;; show a fresh new buffer
  `(let ((name ,name))
     (ignore-errors (kill-buffer name))
     (with-current-buffer (switch-to-buffer name)
       ;; setup keymap
       (let ((keymap (make-sparse-keymap)))
         (set-keymap-parent keymap ,keymap)
         (suppress-keymap keymap)
         (define-key keymap (kbd "q")
           (lambda () (interactive) ,on-exit))
         (use-local-map keymap))
       ;; user body
       (let ((inhibit-read-only t)) ,@body)
       (set-buffer-modified-p nil))))

(defun comb--prompt-save-value (prompt value &optional path)
  "Utility to save VALUE to PATH.

If PATH is nil then PROMPT is used to ask the user."
  (let ((path (or path (read-file-name-default prompt))))
    (if (and (file-writable-p path)
             (not (file-directory-p path)))
        (when (or (not (file-exists-p path))
                  (yes-or-no-p (format "Really overwrite %s? " path)))
          (with-temp-file path (prin1 value (current-buffer))) path)
      (message "Cannot access %s" path) nil)))

(defun comb--prompt-load-value (prompt &optional path)
  "Utility to load a value from PATH.

If PATH is nil then PROMPT is used to ask the user."
  (let ((path (or path (read-file-name-default prompt))))
    (if (and (file-readable-p path)
             (not (file-directory-p path)))
        (with-temp-buffer
          (insert-file-contents path)
          (ignore-errors (cons path (read (current-buffer)))))
      (message "Cannot access %s" path) nil)))

(provide 'comb-common)

;;; comb-common.el ends here
