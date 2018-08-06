;;; comb.el --- Interactive grep annotation tool -*- lexical-binding: t -*-

;; TODO add license and bump version

;; Author: Andrea Cardaci <cyrus.and@gmail.com>
;; Version: 0.0.0
;; URL: https://github.com/cyrus-and/comb
;; Package-Requires: ((emacs "24.4"))
;; Keywords: matching

;;; Commentary:

;; Comb is a native Emacs Lisp solution to search, explore and annotate
;; occurrences of regular expressions in files.  The interactive interface
;; allows to perform an exhaustive classification of all the results to rule out
;; false positives and asses proper matches.

;;; Code:

(require 'comb-browse)

(defgroup comb nil
  "Interactive grep annotation tool."
  :group 'matching)

;;;###autoload
(defun comb ()
  "Start comb!"
  (interactive)
  (comb--browse))

(provide 'comb)

;;; comb.el ends here
