;;; comb-setup.el --- Configuration buffer -*- lexical-binding: t -*-

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

(require 'comb-common)
(require 'comb-search)
(require 'comb-session)

(require 'wid-edit)
(require 'seq)

(declare-function comb--display "comb-browse")

(defvar comb--root-widget)
(defvar comb--patterns-widget)
(defvar comb--callbacks-widget)
(defvar comb--include-files-widget)
(defvar comb--exclude-paths-widget)

(defvar comb-configure-mode-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap widget-keymap)
    (define-key keymap (kbd "R") #'comb--configuration-load-ui)
    (define-key keymap (kbd "S") #'comb--configuration-search)
    (define-key keymap (kbd "q") #'comb--configuration-quit)
    keymap)
  "Keymap for comb configuration")

(define-derived-mode comb-configure-mode special-mode "Comb"
  "Major mode for Comb configuration.

\\{comb-configure-mode-map}"
  (setq buffer-read-only nil))

(defun comb--configure ()
  "Show the configuration buffer."
  (comb--with-temp-buffer-window
   "*Comb: configure*"
   ;; major mode
   comb-configure-mode
   ;; add root directory
   (widget-insert "In directory:\n\n")
   (setq comb--root-widget
         (comb--create-directory-widget "Search root directory"))
   (widget-insert "\n")
   ;; add regexp lists
   (widget-insert "Search for regexps:\n\n")
   (setq comb--patterns-widget
         (comb--create-regex-list-widget "\\<word\\>"
                                         "Search files for this regexp"))
   (widget-insert "\n")
   (widget-insert "In files matching regexps:\n\n")
   (setq comb--include-files-widget
         (comb--create-regex-list-widget "\\.extension$"
                                         "Match only the file name"))
   (widget-insert "\n")
   (widget-insert "Skipping directories matching regexps:\n\n")
   (setq comb--exclude-paths-widget
         (comb--create-regex-list-widget "^some/directory$"
                                         "Match only the directory name"))
   (widget-insert "\n")
   (widget-insert "Including results from callbacks:\n\n")
   (setq comb--callbacks-widget
         (comb--create-function-list-widget 'some-callback
                                            "Function or lambda"))
   (widget-insert "\n\n")
   ;; add search and reset buttons
   (comb--create-button-widget "Reset" #'comb--configuration-load-ui
                               "Drop unsaved modifications")
   (widget-insert " ")
   (comb--create-button-widget "Search" #'comb--configuration-search
                               "Perform a new search")
   (widget-insert "\n")
   ;; finalize
   (widget-setup)
   (comb--configuration-load-ui)
   (goto-char (point-min))))

(defun comb--create-list-widget (item)
  "Editable list widget of ITEM."
  (let (widget)
    ;; create the list
    (setq widget
          (widget-create
           'editable-list
           :entry-format "%d %v"
           :delete-button-args '(:tag "-")
           :append-button-args '(:tag "+")
           `(cons :format "%v"
                  ;; [] consistency with buttons
                  (toggle :format ,(format "%%[%s%%v%s%%]"
                                           widget-push-button-prefix
                                           widget-push-button-suffix)
                          :on "✓" :off "✗" :value t
                          :help-echo "Toggle this item")
                  ,item)))
    ;; create import/export buttons
    (comb--create-button-widget "Import" (comb--configuration-import widget)
                                "Import values from file and add them to these")
    (widget-insert " ")
    (comb--create-button-widget "Export" (comb--configuration-export widget)
                                "Export these values to file")
    (widget-insert "\n")
    widget))

(defun comb--create-regex-list-widget (placeholder help)
  "Editable regexp list widget."
  (comb--create-list-widget
   `(regexp :format " %v" :value ,placeholder :help-echo ,help)))

(defun comb--create-function-list-widget (placeholder help)
  "Editable regex or function list widget."
  (comb--create-list-widget
   `(sexp :format " %v" :value ,placeholder :help-echo ,help)))

(defun comb--create-button-widget (tag action help)
  "Button widget given TAG and ACTION."
  (widget-create 'push-button
                 :tag tag
                 :notify (lambda (&rest _) (funcall action))
                 :help-echo help))

(defun comb--create-directory-widget (help)
  "Directory input widget."
  (widget-create 'directory :format "%v" :help-echo help))

(defun comb--pattern-list-merge (pattern-list)
  "Merge PATTERN-LIST into one regexp that matches any of them."
  (let (enabled)
    (setq enabled (comb--editable-list-filter pattern-list))
    (when enabled
      (format "\\(%s\\)" (mapconcat #'identity enabled "\\|")))))

(defun comb--editable-list-filter (editable-list)
  "Extract enabled items from EDITABLE-LIST.

EDITABLE-LIST is a cons list in the form (ENABLED . ITEM), only
those that have ENABLED non-nil and ITEM non-empty are included
in the result."
  (mapcar #'cdr (seq-filter
                 (lambda (item) (and (car item) (not (equal (cdr item) ""))))
                 editable-list)))

;; configuration commands

(defun comb--configuration-load-ui ()
  "Populate the GUI using the current session."
  (interactive)
  (save-mark-and-excursion
    (widget-value-set comb--root-widget (comb--root))
    (widget-value-set comb--patterns-widget (comb--patterns))
    (widget-value-set comb--callbacks-widget (comb--callbacks))
    (widget-value-set comb--include-files-widget (comb--include-files))
    (widget-value-set comb--exclude-paths-widget (comb--exclude-paths))
    (widget-setup)
    (set-buffer-modified-p nil)))

(defun comb--configuration-save-ui ()
  "Apply the GUI changes to the current session."
  (setf (comb--root) (widget-value comb--root-widget))
  (setf (comb--patterns) (widget-value comb--patterns-widget))
  (setf (comb--callbacks) (widget-value comb--callbacks-widget))
  (setf (comb--include-files) (widget-value comb--include-files-widget))
  (setf (comb--exclude-paths) (widget-value comb--exclude-paths-widget))
  (set-buffer-modified-p nil))

(defun comb--configuration-search ()
  "Start a new search from the configuration buffer."
  (interactive)
  (comb--configuration-save-ui)
  (redisplay) ; allow to show the unmodified mark immediately
  (if (comb--search (comb--pattern-list-merge (comb--patterns))
                    (comb--editable-list-filter (comb--callbacks))
                    (comb--pattern-list-merge (comb--include-files))
                    (comb--pattern-list-merge (comb--exclude-paths)))
      (progn (kill-buffer) (comb--display))
    (comb--kill-main-buffer)))

(defun comb--configuration-quit ()
  "Quit the configuration buffer committing changes to the session."
  (interactive)
  (comb--configuration-save-ui)
  (kill-buffer))

(defun comb--configuration-import (widget)
  "Import patterns for WIDGET from file."
  (lambda ()
    (let (result)
      (setq result (comb--prompt-load-value "List file: "))
      (when (car result)
        (widget-value-set widget (append (widget-value widget) (cdr result)))
        (widget-setup)
        (message "Values added from %s" (car result))))))

(defun comb--configuration-export (widget)
  "Export the patterns of WIDGET to file."
  (lambda ()
    (let (path)
      (setq path (comb--prompt-save-value "List file: " (widget-value widget)))
      (when path
        (message "Values saved to %s" path)))))

(provide 'comb-configure)

;;; comb-configure.el ends here
