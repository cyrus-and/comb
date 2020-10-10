;;; comb-search.el --- Search operations -*- lexical-binding: t -*-

;; Copyright (c) 2020 Andrea Cardaci <cyrus.and@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'comb-session)

(require 'seq)

(defun comb--search (pattern callbacks include-file exclude-path)
  "Perform the lookup of PATTERN and execute CALLBACKS according to the session.

See `comb-find' for the meaning of arguments.

In doing so the cursor is reset to the beginning."
  (let (results)
    (if (or pattern callbacks)
        (condition-case err
            (progn
              (setq results (comb--find-grep pattern callbacks (comb--root)
                                             include-file exclude-path))
              ;; replace the results
              (setf (comb--results) results)
              (setf (comb--cursor) -1)
              (if (seq-empty-p results)
                  (progn (message "No results") nil)
                (message "Found %s results" (length results))))
          (quit (message "Search aborted") nil)
          (file-error (message (error-message-string err)) nil))
      (message "No pattern specified") nil)))

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
        (setq output (nconc output partial))))))

(defun comb--grep (pattern buffer)
  "Return the list of all the occurrences of PATTERN in BUFFER.

The list is a list of conses in the form (BEGIN . END) in point
coordinates.

Note the returned list is in reversed order."
  ;; disable case-insensitive search
  (let (output (case-fold-search nil))
    (with-current-buffer buffer
      ;; lookup the next pattern until EOF
      (condition-case nil
          (while (re-search-forward pattern nil t)
            (push (cons (match-beginning 0) (match-end 0)) output)
            ;; synthetically advance the point for zero-width results, e.g., ^$
            (when (= (match-beginning 0) (match-end 0))
              (forward-char)))
        ;; exit on EOF due to `forward-char'
        (end-of-buffer)))
    ;; return the list of all the occurrences
    output))

(defun comb--find-grep (pattern callbacks root &optional include-file exclude-path)
  "Search PATTERN and apply CALLBACKS in all the matching files in ROOT.

CALLBACKS is a list of functions each taking a filename and a
buffer as arguments and returning a list of occurrences just like
`comb--grep' does.

This is basically a composition of `comb--find' and `comb--grep'
but returns a vector of results in the form (PATH . (BEGIN
. END)).

If ROOT is not an absolute path then it is considered relative to
the filesystem root directory.

The failure to open a file for reading is not fatal and is
reported to *Messages*. The same goes for CALLBACKS errors."
  (let (default-directory i file-list file-list-length output
         occurrences callbacks-results)
    ;; root defaults to the file system root directory (note, default-directory
    ;; is nil here)
    (setq default-directory (expand-file-name root))
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
      ;; load the file in a temporary buffer
      (condition-case err
          (with-temp-buffer
            (insert-file-contents-literally path)
            ;; XXX this is faster than `decode-coding-inserted-region' but it
            ;; may result in a wrong guesswork theoretically (still not
            ;; reproducible in practice); also see comb-report.el
            (decode-coding-region (point-min) (point-max) 'undecided)
            ;; find the occurrences for the current file catching errors
            (when pattern
              (setq occurrences (comb--grep pattern (current-buffer))))
            ;; apply callbacks to the file and append also those results
            (setq callbacks-results
                  (mapcan
                   (lambda (callback)
                     (goto-char (point-min))
                     ;; just notify errors for callbacks errors
                     (with-demoted-errors
                         (funcall callback path)))
                   callbacks)))
        ;; just notify errors for unreadable files
        (file-error (message (error-message-string err))))
      ;; append the results sorted according to the match beginning
      (dolist (position (sort (nconc occurrences callbacks-results)
                              (lambda (x y) (< (car x) (car y)))))
        (push (cons path position) output)))
    ;; return the vector of all the results
    (nreverse (vconcat output))))

(provide 'comb-search)

;;; comb-search.el ends here
