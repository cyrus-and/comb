;;; comb-session.el --- Session management -*- lexical-binding: t -*-

;;; Code:

(require 'comb-common)

(require 'cl-macs)

(defvar comb--session-file nil
  "Remember the loaded session so to not ask again on save.")

;; XXX using a vector underlying type (instead of records) allows the session
;; file to be portable because explicitly setting the :name option forces the
;; tag (the first item of the vector) to be always the name of the structure
;; (instead of some version-dependent name, i.e., prefixed by cl-struct- in
;; Emacs 25)
(cl-defstruct (comb--session (:type vector) :named)
  (root (abbreviate-file-name default-directory))
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
  (if (or (null comb--session)
          (yes-or-no-p "Really discard the current session? "))
      (progn
        (setq comb--session-file nil)
        (setq comb--session (make-comb--session))
        (message "Session created with root %s" (comb--root)) t)
    (message "Session not created") nil))

(defun comb--session-load ()
  "Load a session from file."
  (let (result path session)
    (setq result (comb--prompt-load-value "Session file: "))
    (setq path (car result))
    (setq session (cdr result))
    (when (and result
               (or (null comb--session)
                   (yes-or-no-p "Really discard the current session? ")))
      (if (comb--session-p session)
          (progn
            (setq comb--session-file path)
            (setq comb--session session)
            (message "Session loaded from %s" path) t)
        (message "Invalid session file %s" path) nil))))

(defun comb--session-save ()
  "Save the current session to file."
  (let (path)
    (setq path (comb--prompt-save-value
                "Session file: " comb--session comb--session-file))
    (when path
      (setq comb--session-file path)
      (message "Session saved to %s" path))))

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

(provide 'comb-session)

;;; comb-session.el ends here
