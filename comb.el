;;; comb.el --- Interactive grep annotation tool for manual static analysis -*- lexical-binding: t -*-

;; TODO add license and bump version

;; Author: Andrea Cardaci <cyrus.and@gmail.com>
;; Version: 0.0.0
;; URL: https://github.com/cyrus-and/comb
;; Package-Requires: ((emacs "25.1"))
;; Keywords: matching

;;; Commentary:

;; Comb is a native Emacs Lisp solution to search, explore and annotate
;; occurrences of regular expressions in files.  The interactive interface
;; allows to perform an exhaustive classification of all the results to rule out
;; false positives and asses proper matches.

;;; Code:

(require 'comb-browse)

(defgroup comb nil
  "Interactive grep annotation tool for manual static analysis."
  :group 'matching)

;;;###autoload
(defun comb ()
  "Start combing!

Start a new interactive session using `default-directory' as the
search root directory or resume an existing one."
  (interactive)
  (comb--browse))

(provide 'comb)

;;; comb.el ends here
