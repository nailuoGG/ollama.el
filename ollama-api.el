;;; ollama-api.el --- Ollama API communication -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jiale.liu

;; Author: jiale.liu <im@liujiale.me>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (request "0.3.0") (json "1.8"))
;; Keywords: ollama, api
;; URL: https://github.com/nailuoGG/ollama.el

;;; Commentary:
;; Low-level API communication layer. Provides:
;; - Base API request function with error handling
;; - Model data fetching utilities
;; - Configuration for API endpoint
;; See also: `ollama' for high-level operations, `ollama-status' for model display

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

(defcustom ollama-api-timeout 30
  "Timeout in seconds for Ollama API requests.
Increase this value if you experience timeout errors when pulling large models."
  :type 'integer
  :group 'ollama-api)

(defun ollama--api-request (endpoint &optional method data callback &rest args)
  "Make a request to Ollama API at ENDPOINT.
Optional METHOD specifies the HTTP method (default: \"GET\").
Optional DATA is the request payload.
Optional CALLBACK is called with the response data on success.
Optional ARGS may contain :error keyword for custom error handling."
  (let ((error-callback (plist-get args :error)))
    (condition-case err
        (request (concat ollama-api-url endpoint)
                 :type (or method "GET")
                 :headers '(("Content-Type" . "application/json"))
                 :data (when data 
                         (condition-case json-err
                             (json-encode data)
                           (error
                            (let ((msg (format "JSON encoding error: %s" (error-message-string json-err))))
                              (if error-callback
                                  (funcall error-callback msg)
                                (user-error "%s" msg))
                              nil))))
                 :parser (lambda ()
                           (let ((raw-response (buffer-string)))
                             (cond
                              ;; For DELETE requests, allow empty responses
                              ((and (string= method "DELETE") (string-empty-p (string-trim raw-response)))
                               '((success . t)))
                              ;; Try to parse as JSON
                              (t
                               (condition-case json-err
                                   (json-read)
                                 (json-error
                                  (if (string= method "DELETE")
                                      ;; For DELETE, treat any response as success if we got here
                                      '((success . t))
                                    (message "JSON parsing error: %s. Raw response: %s" 
                                             (error-message-string json-err)
                                             raw-response)
                                    nil))
                                 (error
                                  (message "Error parsing response: %s" (error-message-string json-err))
                                  nil))))))
                 :timeout ollama-api-timeout
                 :success (cl-function
                           (lambda (&key data &allow-other-keys)
                             (cond
                              ;; For DELETE requests, null data is acceptable
                              ((and (string= method "DELETE") (null data))
                               (when callback
                                 (funcall callback '((success . t)))))
                              ;; For other requests, null data is an error
                              ((null data)
                               (let ((msg "API returned invalid JSON response"))
                                 (if error-callback
                                     (funcall error-callback msg)
                                   (message "%s" msg))))
                              ;; Normal case with data
                              (t
                               (when callback
                                 (funcall callback data))))))
                 :error (cl-function
                         (lambda (&key error-thrown response &allow-other-keys)
                           (let ((status-code (request-response-status-code response))
                                 (msg (format "Ollama API Error: %s (HTTP %s)" 
                                              error-thrown 
                                              (or status-code "unknown"))))
                             (if error-callback
                                 (funcall error-callback msg)
                               (user-error "%s" msg))))))
      (error
       (let ((msg (format "Request error: %s" (error-message-string err))))
         (if error-callback
             (funcall error-callback msg)
           (user-error "%s" msg)))))))

(defun ollama--check-server (&optional callback)
  "Check if the Ollama server is running.
If CALLBACK is provided, call it with t if server is running, nil otherwise."
  (request (concat ollama-api-url "/api/version")
    :type "GET"
    :parser 'json-read
    :timeout 5
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (when callback
                  (funcall callback t))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (let ((msg (format "Ollama server not running or unreachable at %s. Error: %s"
                                 ollama-api-url error-thrown)))
                (message "%s" msg)
                (when callback
                  (funcall callback nil)))))))

(defun ollama--get-local-models (&optional callback)
  "Get list of locally installed Ollama models.
If CALLBACK is provided, call it with the models data.
Returns the models list if called synchronously."
  (ollama--check-server
   (lambda (server-running)
     (if (not server-running)
         (progn
           (message "Cannot fetch models: Ollama server not running")
           (when callback
             (funcall callback nil)))
       (ollama--api-request "/api/tags"
                            "GET"
                            nil
                            (lambda (data)
                              (condition-case err
                                  (let ((models (or (cdr (assoc 'models data)) '())))
                                    (if callback
                                        (funcall callback models)
                                      models))
                                (error
                                 (message "Error processing model data: %s" (error-message-string err))
                                 (if callback
                                     (funcall callback nil)
                                   nil))))
                            :error (lambda (err)
                                     (message "Failed to fetch models: %s" err)
                                     (if callback
                                         (funcall callback nil)
                                       nil)))))))

(provide 'ollama-api)
;;; ollama-api.el ends here
