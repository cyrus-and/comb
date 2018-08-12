;;; comb-session.el --- Session management -*- lexical-binding: t -*-

;;; Code:

(require 'cl-macs)

(defvar comb--session-file nil
  "Remember the loaded session so to not ask again on save.")

;; XXX using a vector underlying type (instead of records) allows the session
;; file to be portable because explicitly setting the :name option forces the
;; tag (the first item of the vector) to be always the name of the structure
;; (instead of some version-dependent name, i.e., prefixed by cl-struct- in
;; Emacs 25)
(cl-defstruct (comb--session (:type vector) :named)
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

(defvar comb--session nil
  "Global session handler.")

(defun comb--session-new ()
  "Initialize a new empty session."
  (if (yes-or-no-p "Really discard the current session? ")
      (progn
        (setq comb--session-file nil)
        (setq comb--session (make-comb--session))
        (message "Session created with root %s" (comb--root)) t)
    (message "Session not created")))

(defun comb--session-load ()
  "Load a session from file."
  (let (path session)
    (setq path (read-file-name "Session file: "))
    (if (and (file-readable-p path)
             (not (file-directory-p path)))
        (if (yes-or-no-p "Really discard the current session? ")
            (with-temp-buffer
              (insert-file-contents path)
              (ignore-errors (setq session (read (current-buffer))))
              (if (comb--session-p session)
                  (progn
                    (setq comb--session-file path)
                    (setq comb--session session)
                    (message "Session loaded from %s" path) t)
                (message "Invalid session file %s" path) nil))
          (message "Session not loaded"))
      (message "Cannot access %s" path) nil)))

(defun comb--session-save ()
  "Save the current session to file."
  (let (path)
    (setq path (or comb--session-file
                   (read-file-name-default "Session file: ")))
    (if (and (file-writable-p path)
             (not (file-directory-p path)))
        (if (or (not (file-exists-p path))
                (yes-or-no-p (format "Really overwrite %s? " path)))
            (with-temp-file path
              (prin1 comb--session (current-buffer))
              (setq comb--session-file path)
              (message "Session saved to %s" path) t)
          (message "Session not saved"))
      (message "Cannot access %s" path) nil)))

(provide 'comb-session)

;;; comb-session.el ends here
