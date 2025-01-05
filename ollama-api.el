;;; ollama-api.el --- Ollama API communication -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jiale.liu

;; Author: jiale.liu <im@liujiale.me>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (request "0.3.0") (json "1.8"))
;; Keywords: ollama, api
;; URL: https://github.com/nailuoGG/ollama.el

;;; Commentary:
;; This file contains the low-level API communication functions for Ollama.

;;; Code:

(require 'request)
(require 'json)

(defgroup ollama-api nil
  "Ollama API communication."
  :group 'ollama)

(defcustom ollama-api-url "http://localhost:11434"
  "Base URL for Ollama API."
  :type 'string
  :group 'ollama-api)

(defun ollama--api-request (endpoint &optional method data callback &rest args)
  "Make a request to Ollama API.
Optional ARGS may contain :error keyword for error handling."
  (let ((error-callback (plist-get args :error)))
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
                (if error-callback
                    (funcall error-callback error-thrown)
                  (message "Ollama error: %s" error-thrown)))))))

(defun ollama--get-local-models (&optional callback)
  "Get list of locally installed Ollama models.
If CALLBACK is provided, call it with the models data.
Returns the models list if called synchronously."
  (ollama--api-request "/api/tags"
                       "GET"
                       nil
                       (lambda (data)
                         (let ((models (cdr (assoc 'models data))))
                           (if callback
                               (funcall callback models)
                             models)))))

(provide 'ollama-api)
;;; ollama-api.el ends here
