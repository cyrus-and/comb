;;; comb-report.el --- Report view -*- lexical-binding: t -*-

;;; Code:

(require 'comb-common)
(require 'comb-filter)
(require 'comb-session)

(declare-function comb--display "comb-browse")

(defface comb-cursor '((t :inherit highlight))
  "Face used to highlight the entries in the report buffer."
  :group 'comb)

(defconst comb--max-context 100
  "Maximum number chars before and after the match to display in the report.")

(defun comb--report ()
  "Show the results in a report format."
  (let (overlay result info snippet point)
    (comb--with-temp-buffer-window
     "*Comb: report*"
     ;; on quit
     (kill-buffer)
     ;; keymap
     (let (keymap)
       (setq keymap (make-sparse-keymap))
       (define-key keymap (kbd "RET")
         (lambda () (interactive) (comb--visit-snippet)))
       keymap)
     ;; setup
     (setq truncate-lines nil)
     (setq buffer-read-only t)
     (cursor-sensor-mode)
     (setq overlay (make-overlay 0 0))
     (overlay-put overlay 'face 'comb-cursor)
     ;; populate the buffer
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
  (let (path begin end file-ok)
    ;; prepare variables
    (setq path (concat (file-name-as-directory (comb--root)) (car result)))
    (setq begin (cadr result))
    (setq end (cddr result))
    (with-temp-buffer
      ;; attempt to load the file and mark the region (errors if file shrinked
      ;; or deleted)
      (ignore-errors
        (insert-file-contents-literally path)
        (set-text-properties begin end '(face comb-match))
        (setq file-ok t))
      (goto-char begin)
      ;; format the result
      (concat
       ;; status flag
       (comb--format-status (car info)) " "
       ;; file location
       (if file-ok
           (comb--format-file-location path (line-number-at-pos))
         (propertize path 'face 'error))
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
  (let (cursor)
    (setq cursor (get-text-property (point) 'cursor))
    (if (null cursor)
        (message "No result under the point")
      (setf (comb--cursor) cursor)
      (comb--display))))

(provide 'comb-report)

;;; comb-report.el ends here
