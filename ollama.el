;;; ollama.el --- Manage Ollama models from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jiale.liu

;; Author: jiale.liu <im@liujiale.me>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (ollama-api "0.1") (ollama-status "0.1"))
;; Keywords: ollama, ai, models
;; URL: https://github.com/nailuoGG/ollama.el

;;; Commentary:
;; This package provides an interface to manage Ollama models from Emacs.

;;; Code:

(require 'ollama-api)
(require 'ollama-utils)

(defgroup ollama nil
  "Ollama model management."
  :group 'tools)

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
  "Delete MODEL-NAME from Ollama.
If called interactively, prompt for model name with completion."
  (interactive
   (list (ollama-select-model)))
  (ollama--api-request "/api/delete"
                       "DELETE"
                       `((name . ,model-name))
                       (lambda (data)
                         (message "Deleted model: %s" model-name))))

;;;###autoload
(defun ollama-select-model ()
  "Select an Ollama model from local models using completing-read."
  (interactive)
  (let ((models nil)
        (done nil))
    (ollama--get-local-models
     (lambda (model-data)
       (setq models (mapcar (lambda (model)
                              (alist-get 'name model))
                            model-data))
       (setq done t)))
    ;; Wait for the async request to complete
    (while (not done)
      (sit-for 0.1))
    (completing-read "Select model: " models)))

;;;###autoload
(defun ollama-show-model (model-name)
  "Show information about MODEL-NAME.
If called interactively, prompt for model name with completion."
  (interactive
   (list (ollama-select-model)))
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
  "Copy SOURCE model to DESTINATION.
If called interactively, prompt for source and destination model names with completion."
  (interactive
   (list (ollama-select-model)
         (read-string "Destination model name: ")))
  (ollama--api-request "/api/copy"
                       "POST"
                       `((source . ,source)
                         (destination . ,destination))
                       (lambda (data)
                         (message "Copied %s to %s" source destination))))

(provide 'ollama)
;;; ollama.el ends here
