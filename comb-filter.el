;;; comb-filter.el --- Result filtering -*- lexical-binding: t -*-

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

(require 'subr-x)

(defun comb--filter (result)
  "Filter RESULT according to the current session filters."
  (let (info)
    (setq info (comb--get-info result))
    (and (or (eq (comb--status-filter) t)
             (eq (comb--status-filter) (car info)))
         (or (string-empty-p (comb--notes-filter))
             (and (cdr info)
                  (string-match-p (comb--notes-filter) (cdr info)))))))

;; XXX this is a potentially slow operation
(defun comb--count-results ()
  "Walk the results and count those matching the filter.

Convert the cursor accordingly and return (INDEX . COUNT) where
INDEX can be nil if the current result does not match the
filter."
  (let (index count)
    (setq index -1)
    (setq count 0)
    ;; apply the filter to all the results and count those passing
    (dotimes (i (length (comb--results)))
      (when (funcall #'comb--filter (comb--get-result i))
        (setq count (1+ count))
        ;; also count how many results pass the filter before the cursor
        (when (<= i (comb--cursor))
          (setq index (1+ index)))))
    ;; return a nil index if the current result does not match the filter
    (if (and (comb--valid-cursor-p)
             (not (funcall #'comb--filter (comb--get-result))))
        (cons nil count)
      (cons index count))))

(provide 'comb-filter)

;;; comb-filter.el ends here
