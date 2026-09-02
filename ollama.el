;;; ollama.el --- Manage Ollama models from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jiale.liu

;; Author: jiale.liu <im@liujiale.me>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: ollama, ai, models
;; URL: https://github.com/nailuoGG/ollama.el

;;; Commentary:
;; Main entry point for Ollama model management. Provides:
;; - Core model operations (pull, delete, copy, show)
;; - Model selection interface
;; - Integration with status view and transient menu
;; See also: `ollama-api' for API communication, `ollama-status' for model dashboard

;;; Code:

(require 'ollama-api)
(require 'ollama-utils)

(defgroup ollama nil
  "Ollama model management."
  :group 'tools)

;;;###autoload
(defun ollama-pull-model (model-name)
  "Pull a new MODEL-NAME from Ollama.
Shows progress messages during the pull operation."
  (interactive "sModel name: ")
  (message "Starting pull of model: %s" model-name)
  (ollama--api-request "/api/pull"
                       "POST"
                       `((model . ,model-name))
                       (lambda (data)
                         (let ((status (alist-get 'status data))
                               (completed (alist-get 'completed data)))
                           (if completed
                               (message "Model %s pulled successfully" model-name)
                             (message "Pulling model %s: %s" 
                                      model-name 
                                      (or status "in progress")))))
                       :error (lambda (err)
                                (message "Failed to pull model %s: %s" model-name err))))

;;;###autoload
(defun ollama-delete-model (model-name)
  "Delete MODEL-NAME from Ollama.
If called interactively, prompt for model name with completion."
  (interactive
   (list (ollama-select-model)))
  
  ;; Check if model-name is nil or empty
  (if (or (null model-name) (string-empty-p model-name))
      (user-error "Model name cannot be empty")
    
    ;; Model name is valid, proceed with deletion
    (message "Deleting model: %s..." model-name)
    (condition-case err
        (ollama--api-request "/api/delete"
                           "DELETE"
                           `((model . ,model-name))
                           (lambda (data)
                             (message "Model %s deleted successfully" model-name))
                           :error (lambda (err)
                                    (message "Failed to delete model %s: %s" model-name err)))
      (error
       (message "Error in delete request: %s" (error-message-string err))))))

;;;###autoload
(defun ollama-select-model (&optional callback)
  "Select an Ollama model from local models using completing-read.
If CALLBACK is provided, call it with the selected model name.
When used interactively or with a callback, this function handles the
asynchronous nature of the API request properly.

Note: This function cannot be used synchronously in non-interactive code
due to its asynchronous nature. Always provide a callback when using
programmatically."
  (interactive)
  (message "Fetching available models...")
  ;; First check if the server is running
  (ollama--check-server
   (lambda (server-running)
     (if (not server-running)
         (progn
           (message "Cannot select model: Ollama server not running")
           (when callback
             (funcall callback nil)))
       ;; Server is running, proceed to get models
       (ollama--get-local-models
        (lambda (model-data)
          (condition-case err
              (let* ((models (mapcar (lambda (model)
                                       (alist-get 'name model))
                                     (or model-data '()))))
                (if (null models)
                    (progn
                      (message "No models available. Use M-x ollama-pull-model to download a model")
                      (when callback
                        (funcall callback nil)))
                  ;; We have models, proceed with selection
                  (if (called-interactively-p 'any)
                      ;; Interactive use - prompt user
                      (let ((selected (completing-read "Select model: " models)))
                        (if callback
                            (funcall callback selected)
                          selected))
                    ;; Non-interactive use - must have callback
                    (if callback
                        (funcall callback (car models)) ; Default to first model for non-interactive use
                      (message "Warning: ollama-select-model called non-interactively without callback")
                      nil))))
            (error
             (message "Error selecting model: %s" (error-message-string err))
             (when callback
               (funcall callback nil))
             nil)))))))
  ;; Always return nil immediately for non-interactive use
  ;; The actual result will be delivered via the callback
  nil)

;;;###autoload
(defun ollama-show-model (model-name)
  "Show information about MODEL-NAME.
If called interactively, prompt for model name with completion."
  (interactive
   (list (ollama-select-model)))
  
  ;; Check if model-name is nil or empty
  (if (or (null model-name) (string-empty-p model-name))
      (user-error "Model name cannot be empty")
    
    ;; Model name is valid, proceed with showing info
    (message "Fetching information for model: %s..." model-name)
    (condition-case err
        (ollama--api-request "/api/show"
                           "POST"
                           `((model . ,model-name))
                           (lambda (data)
                             (with-current-buffer (get-buffer-create "*Ollama Model Info*")
                               (let ((inhibit-read-only t))
                                 (erase-buffer)
                                 (emacs-lisp-mode)
                                 (insert ";; Model information for: " model-name "\n\n")
                                 (insert (pp-to-string data))
                                 (goto-char (point-min))
                                 (font-lock-ensure)
                                 (setq buffer-read-only t)
                                 (pop-to-buffer (current-buffer))
                                 (message "Showing information for model: %s" model-name))))
                           :error (lambda (err)
                                    (message "Failed to get information for model %s: %s" model-name err)))
      (error
       (message "Error in show model request: %s" (error-message-string err))))))

;;;###autoload
(defun ollama-copy-model (source destination)
  "Copy SOURCE model to DESTINATION.
If called interactively, prompt for source and destination model names with completion."
  (interactive
   (list (ollama-select-model)
         (read-string "Destination model name: ")))
  
  ;; Check if source is nil or empty
  (if (or (null source) (string-empty-p source))
      (user-error "Source model cannot be empty")
    
    ;; Check if destination is nil or empty
    (if (or (null destination) (string-empty-p destination))
        (user-error "Destination model name cannot be empty")
      
      ;; Both source and destination are valid, proceed with copy
      (message "Copying model %s to %s..." source destination)
      (ollama--api-request "/api/copy"
                         "POST"
                         `((source . ,source)
                           (destination . ,destination))
                         (lambda (data)
                           (message "Successfully copied model %s to %s" source destination))
                         :error (lambda (err)
                                  (message "Failed to copy model %s to %s: %s" 
                                           source destination err))))))

;;;###autoload
(defun ollama-check-server ()
  "Check if the Ollama server is running and provide help if it's not."
  (interactive)
  (message "Checking Ollama server status...")
  (ollama--check-server
   (lambda (running)
     (if running
         (message "Ollama server is running at %s" ollama-api-url)
       (when (yes-or-no-p "Ollama server is not running. Would you like to start it?")
         (let ((process (start-process "ollama-server" "*Ollama Server*" "ollama" "serve")))
           (message "Starting Ollama server...")
           (set-process-sentinel
            process
            (lambda (proc event)
              (cond
               ((string-match "finished" event)
                (message "Ollama server stopped"))
               ((string-match "exited abnormally" event)
                (message "Ollama server failed to start: %s" event)))))))))))

(provide 'ollama)
;;; ollama.el ends here
