;;; comb-session.el --- Session management -*- lexical-binding: t -*-

;;; Code:

(require 'cl-macs)

(defvar comb--session-file nil
  "Remember the loaded session so to not ask again on save.")

(cl-defstruct comb--session
  (root default-directory)
  (patterns nil)
  (include-files nil)
  (exclude-paths nil)
  (results [])
  (cursor -1)
  (status-filter t)
  (notes-filter "")
  ;; equality test is needed to load/save the hash table
  (infos (make-hash-table :test #'equal)))

(defmacro comb--root          () '(comb--session-root          comb--session))
(defmacro comb--patterns      () '(comb--session-patterns      comb--session))
(defmacro comb--include-files () '(comb--session-include-files comb--session))
(defmacro comb--exclude-paths () '(comb--session-exclude-paths comb--session))
(defmacro comb--results       () '(comb--session-results       comb--session))
(defmacro comb--cursor        () '(comb--session-cursor        comb--session))
(defmacro comb--status-filter () '(comb--session-status-filter comb--session))
(defmacro comb--notes-filter  () '(comb--session-notes-filter  comb--session))
(defmacro comb--infos         () '(comb--session-infos         comb--session))

(defvar comb--session (make-comb--session)
  "Global session handler.")

(defun comb--session-new ()
  "Initialize a new empty session."
  (when (yes-or-no-p "Really discard the current session? ")
    (setq comb--session-file nil)
    (setq comb--session (make-comb--session))
    (message "Session created with root %s" (comb--root)) t))

(defun comb--session-load ()
  "Load a session from file."
  (let (path session)
    (setq path (read-file-name "Session file: "))
    (if (and (file-readable-p path)
             (not (file-directory-p path)))
        (when (yes-or-no-p "Really discard the current session? ")
          (with-temp-buffer
            (insert-file-contents path)
            (setq session (read (current-buffer)))
            (if (comb--session-p session)
                (progn
                  (setq comb--session-file path)
                  (setq comb--session session)
                  (message "Session loaded from %s" path) t)
              (message "Invalid session file %s" path) nil)))
      (message "Cannot access %s" path) nil)))

(defun comb--session-save ()
  "Save the current session to file."
  (let (path)
    (setq path (or comb--session-file
                   (read-file-name-default "Session file: ")))
    (if (and (file-writable-p path)
             (not (file-directory-p path)))
        (when (or (not (file-exists-p path))
                  (yes-or-no-p "Really overwrite that file? "))
          (with-temp-file path
            (prin1 comb--session (current-buffer))
            (message "Session saved to %s" path)))
      (message "Cannot access %s" path) nil)))

(provide 'comb-session)

;;; comb-session.el ends here
