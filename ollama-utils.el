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
  "Format SIZE in bytes to human readable format (KB, MB, GB)."
  (cond
   ((not (numberp size)) "Unknown")
   ((> size 1000000000) (format "%.1f GB" (/ size 1000000000.0)))
   ((> size 1000000) (format "%.1f MB" (/ size 1000000.0)))
   ((> size 1000) (format "%.1f KB" (/ size 1000.0)))
   (t (format "%d B" size))))

(defun ollama--format-date (date-str)
  "Format DATE-STR to readable format (YYYY-MM-DD HH:MM).
If DATE-STR is invalid, returns 'Unknown date'."
  (condition-case nil
      (format-time-string "%Y-%m-%d %H:%M"
                          (date-to-time date-str))
    (error "Unknown date")))

(defun ollama--sort-size (a b)
  "Compare model sizes for sorting A and B entries."
  (condition-case nil
      (let ((size-a (alist-get 'size (car a)))
            (size-b (alist-get 'size (car b))))
        (if (and (numberp size-a) (numberp size-b))
            (< size-a size-b)
          t))
    (error t)))

(defun ollama--sort-modified (a b)
  "Compare model modified dates for sorting A and B entries."
  (condition-case nil
      (let ((date-a (alist-get 'modified_at (car a)))
            (date-b (alist-get 'modified_at (car b))))
        (if (and (stringp date-a) (stringp date-b))
            (time-less-p (date-to-time date-b)
                         (date-to-time date-a))
          t))
    (error t)))

(defun ollama--prepare-model-entry (model)
  "Prepare MODEL data for tabulated list display.
Handles missing or malformed data gracefully."
  (condition-case nil
      (let* ((details (or (alist-get 'details model) '()))
             (name (or (alist-get 'name model) "Unknown"))
             (size (ollama--format-size (alist-get 'size model)))
             (modified (ollama--format-date (alist-get 'modified_at model)))
             (format (or (alist-get 'format details) "Unknown"))
             (params (or (alist-get 'parameter_size details) "Unknown")))
        (list model (vector name size modified format params)))
    (error
     (list model (vector 
                  (or (alist-get 'name model) "Error")
                  "Unknown" "Unknown" "Unknown" "Unknown")))))

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

(defun ollama--create-display-buffer (buffer-name mode)
  "Create buffer with BUFFER-NAME and MODE according to display rules.
Returns the created buffer. Handles potential errors gracefully."
  (let ((buf (get-buffer-create buffer-name)))
    (condition-case err
        (progn
          (with-current-buffer buf
            (when (not (eq major-mode mode))
              (funcall mode)))
          (display-buffer buf ollama-display-buffer-action)
          buf)
      (error
       (message "Error creating display buffer: %s" (error-message-string err))
       buf))))

(defun ollama--setup-model-buffer (buffer-name mode models)
  "Setup a model buffer with BUFFER-NAME using MODE and MODELS.
This function opens the buffer using the display rules defined in `ollama-display-buffer-action`.
Handles potential errors gracefully."
  (condition-case err
      (let ((buf (ollama--create-display-buffer buffer-name mode)))
        (with-current-buffer buf
          (setq tabulated-list-entries
                (mapcar #'ollama--prepare-model-entry (or models '())))
          (tabulated-list-init-header)
          (tabulated-list-print t)
          (when-let ((win (get-buffer-window buf)))
            (fit-window-to-buffer win))
          buf))
    (error
     (message "Error setting up model buffer: %s" (error-message-string err))
     (get-buffer-create buffer-name))))

(provide 'ollama-utils)
;;; ollama-utils.el ends here
