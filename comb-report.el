;;; comb-report.el --- Report view -*- lexical-binding: t -*-

;;; Code:

(require 'comb-common)
(require 'comb-filter)
(require 'comb-session)

(declare-function comb--browse "comb-browse")

(defconst comb--max-context 100
  "Maximum number chars before and after the match to display in the report.")

(defconst comb--snippet-separator (propertize "--" 'face 'shadow)
  "Text used to separate results in the report.")

(defun comb--report ()
  "Show the reuslts in a report format."
  (let (result info point)
    (comb--with-temp-buffer-window
     "*comb: report*"
     ;; on quit
     (progn (kill-buffer) (comb--browse))
     ;; keymap
     (let (keymap)
       (setq keymap (make-sparse-keymap))
       (define-key keymap (kbd "RET")
         (lambda () (interactive) (comb--visit-snippet)))
       keymap)
     ;; setup
     (setq truncate-lines nil)
     (setq buffer-read-only t)
     ;; allow to easily navigate the report
     (setq paragraph-start (format "^%s$" comb--snippet-separator))
     (if (zerop (cdr (comb--count-results)))
         (insert (format "Nothing to show\n"))
       ;; walk the results and apply the filter
       (insert (format "%s\n" comb--snippet-separator))
       (dotimes (cursor (length (comb--results)))
         (setq result (comb--get-result cursor))
         (when (funcall #'comb--filter result)
           ;; save the point according to the cursor
           (when (= cursor (comb--cursor))
             (setq point (point)))
           ;; prepare the snippet
           (setq info (comb--get-info result))
           ;; set up user actions and insert the snippet
           (insert
            (propertize (comb--format-snippet result info) 'cursor cursor)
            (format "\n%s\n" comb--snippet-separator))))
       ;; finalize
       (goto-char (or point (point-min)))))))

;; TODO possibly retain the original font lock?
(defun comb--format-snippet (result info)
  "Generate the report snippet for RESULT and INFO."
  (let (path begin end)
    ;; prepare variables
    (setq path (concat (file-name-as-directory (comb--root)) (car result)))
    (setq begin (cadr result))
    (setq end (cddr result))
    (with-temp-buffer
      (insert-file-contents-literally path)
      (set-text-properties begin end '(face comb-match))
      (goto-char begin)
      ;; format the result
      (concat
       ;; status flag
       (comb--format-status (car info)) " "
       ;; file location
       (comb--format-file-location path (line-number-at-pos)) "\n"
       ;; notes
       (when (cdr info) (concat (comb--format-notes (cdr info)) "\n"))
       ;; snippet
       (buffer-substring
        (max (- begin comb--max-context)
             (progn (beginning-of-line) (point)))
        (min (+ end comb--max-context)
             (progn (goto-char end) (end-of-line) (point))))))))

(defun comb--visit-snippet ()
  "Visit the snippet under the point."
  (let (cursor)
    (setq cursor (get-text-property (point) 'cursor))
    (if (null cursor)
        (message "No result under the point")
      (setf (comb--cursor) (or cursor (comb--cursor)))
      (comb--browse))))

(provide 'comb-report)

;;; comb-report.el ends here
