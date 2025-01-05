;;; ollama-utils.el --- Ollama utility functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jiale.liu

;; Author: jiale.liu <im@liujiale.me>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (ollama-api "0.1"))
;; Keywords: ollama, utils
;; URL: https://github.com/nailuoGG/ollama.el

;;; Commentary:
;; This file contains utility functions shared across Ollama packages.

;;; Code:

(require 'ollama-api)

(defun ollama--format-size (size)
  "Format SIZE in human readable format."
  (cond
   ((> size 1000000000) (format "%.1f GB" (/ size 1000000000.0)))
   ((> size 1000000) (format "%.1f MB" (/ size 1000000.0)))
   ((> size 1000) (format "%.1f KB" (/ size 1000.0)))
   (t (format "%d B" size))))

(defun ollama--format-date (date-str)
  "Format DATE-STR to readable format."
  (format-time-string "%Y-%m-%d %H:%M"
                      (date-to-time date-str)))

(defun ollama--sort-size (a b)
  "Compare model sizes for sorting."
  (< (alist-get 'size (car a))
     (alist-get 'size (car b))))

(defun ollama--sort-modified (a b)
  "Compare model modified dates for sorting."
  (time-less-p (date-to-time (alist-get 'modified_at (car b)))
               (date-to-time (alist-get 'modified_at (car a)))))

(defun ollama--prepare-model-entry (model)
  "Prepare MODEL data for tabulated list."
  (let* ((details (alist-get 'details model))
         (name (alist-get 'name model))
         (size (ollama--format-size (alist-get 'size model)))
         (modified (ollama--format-date (alist-get 'modified_at model)))
         (format (alist-get 'format details))
         (params (alist-get 'parameter_size details)))
    (list model (vector name size modified format params))))

;; Define display buffer action for Ollama buffers
(defcustom ollama-display-buffer-action
  '((display-buffer-reuse-window
     display-buffer-in-direction)
    (direction . right)
    (window-width . 0.618))
  "Display action for Ollama buffers."
  :type 'sexp
  :group 'ollama)

;; Add to display-buffer-alist for all Ollama buffers
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Ollama" (* any) "*")
               . ,ollama-display-buffer-action))

(defun ollama--setup-model-buffer (buffer-name mode models)
  "Setup a model buffer with BUFFER-NAME using MODE and MODELS.
This function opens the buffer using the display rules defined in `ollama-display-buffer-action`."
  (with-current-buffer (get-buffer-create buffer-name)
    (funcall mode)
    (setq tabulated-list-entries
          (mapcar #'ollama--prepare-model-entry models))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    ;; Use display-buffer with our custom rules
    (let ((window (display-buffer (current-buffer))))
      (select-window window)
      (fit-window-to-buffer window))))

(provide 'ollama-utils)
;;; ollama-utils.el ends here
