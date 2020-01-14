;;; comb.el --- Interactive code auditing and grep tool -*- lexical-binding: t -*-

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

;; Author: Andrea Cardaci <cyrus.and@gmail.com>
;; Version: 0.2.0
;; URL: https://github.com/cyrus-and/comb
;; Package-Requires: ((emacs "25.1"))
;; Keywords: matching

;;; Commentary:

;; Comb is a native Emacs Lisp solution to search, browse and annotate
;; occurrences of regular expressions in files.  The interactive interface
;; allows to perform an exhaustive classification of all the results to rule out
;; false positives and asses proper matches during code audit.

;;; Code:

(require 'comb-browse)
(require 'comb-common)

(defgroup comb nil
  "Interactive code auditing and grep tool"
  :group 'matching)

;;;###autoload
(defun comb ()
  "Start combing!

Start a new interactive session using `default-directory' as the
search root directory or resume an existing one.

All the patterns are Emacs-flavored regexps, see the info
node `(elisp) Regular Expressions'."
  (interactive)
  (comb--save-window-configuration)
  ;; creates a session silently the first time
  (unless comb--session
    (setq comb--session (make-comb--session)))
  (comb--display))

(provide 'comb)

;;; comb.el ends here
