;;; ollama.el --- Manage Ollama models from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jiale.liu

;; Author: jiale.liu <im@liujiale.me>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (request "0.3.0") (json "1.8"))
;; Keywords: ollama, ai, models
;; URL: https://github.com/nailuoGG/ollama

;;; Commentary:
;; This package provides an interface to manage Ollama models from Emacs.

;;; Code:

(require 'request)
(require 'json)

(defgroup ollama nil
  "Ollama model management."
  :group 'tools)

(defcustom ollama-api-url "http://localhost:11434"
  "Base URL for Ollama API."
  :type 'string
  :group 'ollama)

(defun ollama--api-request (endpoint &optional method data callback)
  "Make a request to Ollama API."
  (request (concat ollama-api-url endpoint)
    :type (or method "GET")
    :headers '(("Content-Type" . "application/json"))
    :data (when data (json-encode data))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (when callback (funcall callback data))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Ollama error: %s" error-thrown)))))

(defvar ollama-list-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Normal mode bindings
    (define-key map (kbd "d") 'ollama-delete-model-at-point)
    (define-key map (kbd "s") 'ollama-sort-models)
    (define-key map (kbd "g") 'ollama-list-models)
    ;; Evil mode bindings
    (with-eval-after-load 'evil
      (evil-define-key 'normal ollama-list-mode-map
        "d" 'ollama-delete-model-at-point
        "s" 'ollama-sort-models
        "g" 'ollama-list-models))
    map)
  "Keymap for `ollama-list-mode'.")

(define-derived-mode ollama-list-mode tabulated-list-mode "Ollama Models"
  "Major mode for listing Ollama models."
  (setq tabulated-list-format
        [("Name" 30 t)
         ("Size" 15 ollama--sort-size)
         ("Modified" 20 ollama--sort-modified)
         ("Format" 10 t)
         ("Params" 10 t)])
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header)
  ;; Make buffer read-only
  (setq buffer-read-only t)
  ;; Allow certain commands in read-only buffers
  (setq-local evil-read-only-exempt-commands
              '(ollama-delete-model-at-point
                ollama-sort-models
                ollama-list-models)))

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

(defun ollama-delete-model-at-point ()
  "Delete model at point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (model-name (aref entry 0)))
    (when (yes-or-no-p (format "Delete model %s? " model-name))
      (ollama-delete-model model-name)
      (ollama-list-models))))

(defun ollama-sort-models ()
  "Sort models by current column."
  (interactive)
  (let* ((column (tabulated-list--get-sort-column))
         (sort-fn (aref (tabulated-list-format) column 2)))
    (if sort-fn
        (progn
          (setq tabulated-list-entries
                (sort tabulated-list-entries
                      (lambda (a b)
                        (funcall sort-fn (car a) (car b)))))
          (tabulated-list-print t))
      (message "This column is not sortable"))))

;;;###autoload
(defun ollama-list-models ()
  "List all available models in a tabulated view."
  (interactive)
  (ollama--api-request "/api/tags"
                       "GET"
                       nil
                       (lambda (data)
                         (let ((models (alist-get 'models data)))
                           (with-current-buffer (get-buffer-create "*Ollama Models*")
                             (ollama-list-mode)
                             (setq tabulated-list-entries
                                   (mapcar #'ollama--prepare-model-entry models))
                             (tabulated-list-init-header)
                             (tabulated-list-print t)
                             (pop-to-buffer (current-buffer)))))))

;;;###autoload
(defun ollama-pull-model (model-name)
  "Pull a new MODEL-NAME from Ollama."
  (interactive "sModel name: ")
  (ollama--api-request "/api/pull"
                       "POST"
                       `((name . ,model-name))
                       (lambda (data)
                         (message "Pulling model: %s" model-name))))

;;;###autoload
(defun ollama-delete-model (model-name)
  "Delete MODEL-NAME from Ollama."
  (interactive "sModel name: ")
  (ollama--api-request "/api/delete"
                       "DELETE"
                       `((name . ,model-name))
                       (lambda (data)
                         (message "Deleted model: %s" model-name))))

;;;###autoload
(defun ollama-show-model (model-name)
  "Show information about MODEL-NAME."
  (interactive "sModel name: ")
  (ollama--api-request "/api/show"
                       "POST"
                       `((model . ,model-name))
                       (lambda (data)
                         (with-current-buffer (get-buffer-create "*Ollama Model Info*")
                           (erase-buffer)
                           (insert (pp-to-string data))
                           (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun ollama-copy-model (source destination)
  "Copy SOURCE model to DESTINATION."
  (interactive "sSource model: \nsDestination model: ")
  (ollama--api-request "/api/copy"
                       "POST"
                       `((source . ,source)
                         (destination . ,destination))
                       (lambda (data)
                         (message "Copied %s to %s" source destination))))

(provide 'ollama)
;;; ollama.el ends here
