;;; ollama-status.el --- Ollama status mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jiale.liu

;; Author: jiale.liu <im@liujiale.me>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (ollama-api "0.1"))
;; Keywords: ollama, status
;; URL: https://github.com/nailuoGG/ollama.el

;;; Commentary:
;; Model status dashboard implementation. Provides:
;; - Interactive model management interface
;; - Real-time model list refresh
;; - Sortable tabulated view with model details
;; See also: `ollama-api' for data fetching, `ollama' for core operations

;;; Code:

(require 'ollama-api)
(require 'ollama-utils)
(require 'cl-lib)

;; Forward declarations to avoid circular dependencies
(declare-function ollama-pull-model "ollama")
(declare-function ollama-delete-model "ollama")
(declare-function ollama-show-model "ollama")
(declare-function ollama-copy-model "ollama")

(defgroup ollama-status nil
  "Ollama status view."
  :group 'ollama)

(defvar ollama-status-buffer-name "*Ollama Status*"
  "Name of the buffer used for Ollama status.")

(defvar ollama-status-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Keybindings
    (with-eval-after-load 'evil
      (evil-define-key 'normal ollama-status-mode-map
        "u" 'ollama-status-refresh
        "p" 'ollama-pull-model
        "d" 'ollama-delete-model-at-point
        "c" 'ollama-copy-model
        "i" 'ollama-show-model-info
        "s" 'tabulated-list-sort
        "q" 'quit-window))
    map)
  "Keymap for `ollama-status-mode'.")

(define-derived-mode ollama-status-mode tabulated-list-mode "Ollama Status"
  "Major mode for Ollama status view."
  (setq tabulated-list-format
        [("Name" 40 t)
         ("Size" 15 ollama--sort-size)
         ("Modified" 20 ollama--sort-modified)
         ("Format" 10 t)
         ("Params" 10 t)])
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header)
  (setq-local revert-buffer-function 'ollama-status-refresh)
  (setq-local evil-read-only-exempt-commands
              '(ollama-status-refresh
                ollama-pull-model
                ollama-delete-model-at-point
                ollama-copy-model
                ollama-show-model-info
                ollama-sort-models))
  (hl-line-mode 1)
  (use-local-map ollama-status-mode-map))


(defvar ollama-status--models nil
  "List of models in the current status view.")

(defun ollama-status-refresh (&optional callback)
  "Refresh the Ollama status buffer with latest model data.
Optional CALLBACK is called after successful refresh."
  (interactive)
  (message "Refreshing Ollama models...")
  (ollama--api-request "/api/tags"
                       "GET"
                       nil
                       (lambda (data)
                         (condition-case err
                             (let ((models (or (cdr (assoc 'models data)) '())))
                               (setq ollama-status--models models)
                               (ollama--setup-model-buffer ollama-status-buffer-name 'ollama-status-mode models)
                               (message "Ollama models refreshed successfully (%d models)" (length models))
                               (when callback
                                 (funcall callback)))
                           (error
                            (message "Error processing model data: %s" (error-message-string err)))))
                       :error (lambda (err)
                                (message "Failed to refresh Ollama models: %s" err)
                                (with-current-buffer (get-buffer-create ollama-status-buffer-name)
                                  (let ((inhibit-read-only t))
                                    (erase-buffer)
                                    (insert (format "Error: %s\n\n" err))
                                    (insert "Press 'u' to retry"))))))

;;;###autoload
(defun ollama-list-models ()
  "List all available models in a tabulated view."
  (interactive)
  (ollama-status-refresh))

(defun ollama-sort-models ()
  "Sort models by current column using tabulated-list-mode's built-in sorting."
  (interactive)
  (call-interactively 'tabulated-list-sort))

(defun ollama-status--get-model-at-point ()
  "Get the model at point."
  (let ((entry (tabulated-list-get-entry)))
    (when entry
      (aref entry 0))))

(defun ollama-delete-model-at-point ()
  "Delete the model at point in the Ollama status buffer."
  (interactive)
  (let ((model-name (ollama-status--get-model-at-point)))
    (if (not model-name)
        (user-error "No model at point. Please position cursor on a model first")
      (when (yes-or-no-p (format "Delete model %s? " model-name))
        (message "Deleting model %s..." model-name)
        (condition-case err
            (progn
              (require 'ollama)
              ;; Use a callback to handle the asynchronous nature of the delete operation
              (ollama-delete-model model-name)
              ;; Wait a moment before refresh to ensure deletion completes
              (run-at-time 1.5 nil
                           (lambda ()
                             (ollama-status-refresh
                              (lambda ()
                                (message "Model %s deleted successfully" model-name))))))
          (user-error
           (message "User error deleting model: %s" (error-message-string err)))
          (error
           (message "Error deleting model: %s" (error-message-string err))))))))


(defun ollama-show-model-info ()
  "Show detailed information about the model at point."
  (interactive)
  (let ((model-name (ollama-status--get-model-at-point)))
    (if (not model-name)
        (user-error "No model at point. Please position cursor on a model first")
      (message "Fetching info for model %s..." model-name)
      (condition-case err
          (progn
            (require 'ollama)
            ;; Use a callback approach to handle the asynchronous nature
            (ollama-show-model model-name)
            ;; The buffer will be displayed by ollama-show-model
            )
        (user-error
         (message "User error fetching model info: %s" (error-message-string err)))
        (error
         (message "Error fetching model info: %s" (error-message-string err)))))))

;;;###autoload
(defun ollama-status ()
  "Show Ollama status in a dedicated buffer.
Displays a list of all available models with their details."
  (interactive)
  (condition-case err
      (if (get-buffer ollama-status-buffer-name)
          (progn
            (pop-to-buffer ollama-status-buffer-name)
            (when (y-or-n-p "Refresh model list? ")
              (ollama-status-refresh)))
        (ollama-status-refresh))
    (error
     (message "Error displaying Ollama status: %s" (error-message-string err)))))

(provide 'ollama-status)
;;; ollama-status.el ends here
