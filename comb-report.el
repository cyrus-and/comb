;;; comb-report.el --- Report buffer -*- lexical-binding: t -*-

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

(require 'comb-filter)
(require 'comb-session)
(require 'comb-common)

(declare-function comb--display "comb-browse")

(defface comb-cursor '((t :inherit highlight :extend t))
  "Face used to highlight the entries in the report buffer."
  :group 'comb)

(defconst comb--max-context 100
  "Maximum number chars before and after the match to display in the report.")

(defvar comb-report-mode-map
  (let (keymap)
    (setq keymap (make-sparse-keymap))
    (define-key keymap (kbd "RET") #'comb--visit-snippet)
    keymap)
  "Keymap for Comb results")

(define-derived-mode comb-report-mode special-mode "Comb"
  "Major mode for Comb reports.

\\{comb-report-mode-map}"
  (setq truncate-lines nil)
  (cursor-sensor-mode))

(defun comb--report ()
  "Show the results in a report format."
  (let (overlay result info snippet point)
    (comb--with-temp-buffer-window
     "*Comb: report*"
     ;; major mode
     comb-report-mode
     ;; populate the buffer
     (setq overlay (make-overlay 0 0))
     (overlay-put overlay 'face 'comb-cursor)
     (if (zerop (cdr (comb--count-results)))
         (insert (format "Nothing to show\n"))
       ;; walk the results and apply the filter
       (dotimes (cursor (length (comb--results)))
         (setq result (comb--get-result cursor))
         (when (funcall #'comb--filter result)
           ;; save the point according to the cursor
           (when (= cursor (comb--cursor))
             (setq point (point)))
           ;; prepare the snippet
           (setq info (comb--get-info result))
           (setq snippet (comb--format-snippet result info))
           (let (begin end)
             (setq begin (point))
             (setq end (+ begin (length snippet)))
             ;; set up user actions and insert the snippet
             (insert
              (propertize
               snippet
               'cursor cursor
               'cursor-sensor-functions
               (list (lambda (_window _pos event)
                       ;; move the selection
                       (if (eq event 'entered)
                           (move-overlay overlay begin end)
                         (move-overlay overlay 0 0)))))))))
       ;; finalize
       (goto-char (or point (point-min)))))))

;; TODO possibly retain the original font lock?
(defun comb--format-snippet (result info)
  "Generate the report snippet for RESULT and INFO."
  (let (relative-path path begin end file-ok)
    ;; prepare variables
    (setq relative-path (car result))
    (setq path (concat (file-name-as-directory (comb--root)) (car result)))
    (setq begin (cadr result))
    (setq end (cddr result))
    (with-temp-buffer
      ;; attempt to load the file and mark the region (errors if file shrinked
      ;; or deleted)
      (ignore-errors
        (insert-file-contents-literally path)
        ;; XXX this is faster than `decode-coding-inserted-region' but it
        ;; may result in a wrong guesswork theoretically (still not
        ;; reproducible in practice); also see comb-search.el
        (decode-coding-region (point-min) (point-max) 'undecided)
        (set-text-properties begin end '(face comb-match))
        (setq file-ok t))
      (goto-char begin)
      ;; format the result
      (concat
       ;; status flag
       (comb--format-status (car info)) " "
       ;; file location
       (if file-ok
           (comb--format-file-location relative-path (line-number-at-pos))
         (propertize relative-path 'face 'error))
       "\n"
       ;; notes
       (when (cdr info) (concat (comb--format-notes (cdr info)) "\n"))
       ;; snippet
       (when file-ok
         (buffer-substring
          (max (- begin comb--max-context)
               (progn (beginning-of-line) (point)))
          (min (+ end comb--max-context)
               (progn (goto-char end) (end-of-line) (point)))))
       "\n\n"))))

(defun comb--visit-snippet ()
  "Visit the snippet under the point."
  (interactive)
  (let (cursor)
    (setq cursor (get-text-property (point) 'cursor))
    (if (null cursor)
        (message "No result under the point")
      (setf (comb--cursor) cursor)
      (comb--display))))

(provide 'comb-report)

;;; comb-report.el ends here
