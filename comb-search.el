;;; comb-search.el --- Search operations -*- lexical-binding: t -*-

;;; Code:

(require 'comb-session)

(defun comb--search ()
  "Perform the lookup according to the session and save the results.

In doing so the cursor is reset to the beginning."
  (let (pattern root include-file exclude-path results)
    ;; obtain the values from the session
    (setq root (comb--session-root comb--session))
    (setq pattern
          (comb--merge-regexps (comb--session-patterns comb--session)))
    (setq include-file
          (comb--merge-regexps (comb--session-include-files comb--session)))
    (setq exclude-path
          (comb--merge-regexps (comb--session-exclude-paths comb--session)))
    (if pattern
        ;; perform the search handling errors
        (condition-case err
            (progn
              (setq results
                    (comb--find-grep pattern root include-file exclude-path))
              ;; replace the results
              (setf (comb--session-results comb--session) results)
              (setf (comb--session-cursor comb--session) -1))
          (quit (message "Search aborted") nil)
          (file-error (message (error-message-string err)) nil))
      (message "No pattern specified") nil)))

(defun comb--merge-regexps (regexps)
  "Merge REGEXPS into one regexp that matches any of them.

REGEXP is a cons list in the form (ENABLED . REGEXP), only those
that have ENABLED non-nil and are not empty are included in the
result."
  (let ((enabled
         (seq-filter
          (lambda (item)
            (and (car item) (not (equal (cdr item) ""))))
          regexps)))
    (if enabled
        (format "\\(%s\\)" (mapconcat #'cdr enabled "\\|"))
      nil)))

(defun comb--find (&optional path include-file exclude-path)
  "Walk PATH and return a list of paths.
The paths are relative to `default-directory' if PATH is not an
absolute path.  If PATH empty or nil then `default-directory' is
used.

Only returns files matching the INCLUDE-FILE pattern and descend
into a directory only if the path does not match EXCLUDE-PATH.

Errors during the process are not fatal and are reported to
*Messages* but only if they do not concern PATH, in which case
this function fails."
  (let (output full-path partial)
    ;; treat the empty string as nil so to avoid the ./ prefix
    (when (equal path "")
      (setq path nil))
    ;; walk the files in PATH
    (dolist (entry (directory-files (or path ".")) output)
      ;; build the full path using the parent
      (setq full-path (concat (and path (file-name-as-directory path)) entry))
      ;; build the list of entries to add
      (setq
       partial
       (if (file-directory-p full-path)
           ;; recursively walk the directory if not matches
           (unless (or (equal entry ".") (equal entry "..")
                       (and exclude-path
                            (string-match-p exclude-path full-path)))
             (condition-case err
                 (comb--find full-path include-file exclude-path)
               ;; just notify errors in sub directories
               (file-error (message (error-message-string err)) nil)))
         ;; just return the file entry if matches
         (when (or (not include-file)
                   (string-match-p include-file entry))
           (list full-path))))
      ;; add the partial results in-place, if any
      (when partial
        ;; XXX nconc is O(length-of-all-but-last-list)
        (setq output (nconc output partial))))))

(defun comb--grep (path pattern)
  "Return the list of all the occurrences of PATTERN in PATH.
The list is a list of conses in the form (BEGIN . END) in point
coordinates."
  ;; disable case-insensitive search
  (let (output (case-fold-search nil))
    ;; load the file in a temporary buffer
    (with-temp-buffer
      (insert-file-contents-literally path)
      ;; lookup the next pattern until EOF
      (while (re-search-forward pattern nil t)
        (push (cons (match-beginning 0) (match-end 0)) output)))
    ;; return the list of all the occurrences
    (nreverse output)))

(defun comb--find-grep (pattern &optional root include-file exclude-path)
  "Search PATTERN in all the files in ROOT applying.
This is basically a composition of `comb--find' and `comb--grep'
but returns a vector of results in the form (PATH . (BEGIN . END)).

If ROOT is nil then `default-directory' is used.

The failure to open a file for reading is not fatal and is
reported to *Messages*."
  (let (i file-list file-list-length output occurrences default-directory)
    ;; root defaults to default-directory
    (setq default-directory (expand-file-name (or root ".")))
    ;; initialize stats
    (message "Walking the filesystem...")
    (setq i 0)
    (setq file-list (comb--find nil include-file exclude-path))
    (setq file-list-length (length file-list))
    ;; walk the root directory
    (dolist (path file-list output)
      ;; update counters
      (setq i (1+ i))
      (when (zerop (% i (max (/ file-list-length 100) 1))) ; limit messages
        (let ((message-log-max nil))
          (message "Searching... %s%%"
                   (truncate (* (/ i (float file-list-length)) 100)))))
      ;; find the occurrences for the current file catching errors
      (setq occurrences
            (condition-case err
                (comb--grep path pattern)
              ;; just notify errors for unreadable files
              (file-error (message (error-message-string err)) nil)))
      ;; append the results
      (dolist (position occurrences)
        (push (cons path position) output)))
    ;; return the vector of all the results
    (setq output (nreverse (vconcat output)))
    (message "Found %s results" (length output))
    output))

(provide 'comb-search)

;;; comb-search.el ends here
