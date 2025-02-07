;;; ollama-status.el --- Ollama status mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jiale.liu

;; Author: jiale.liu <im@liujiale.me>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (ollama-api "0.1"))
;; Keywords: ollama, status
;; URL: https://github.com/nailuoGG/ollama.el

;;; Commentary:
;; This file contains the status mode and related functions for Ollama.

;;; Code:

(require 'ollama-api)
(require 'ollama-utils)
(require 'cl-lib)

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
  "Refresh the Ollama status buffer.
Optional CALLBACK is called after successful refresh."
  (interactive)
  (ollama--api-request "/api/tags"
                       "GET"
                       nil
                       (lambda (data)
                         (let ((models (cdr (assoc 'models data))))
                           (setq ollama-status--models models)
                           (ollama--setup-model-buffer ollama-status-buffer-name 'ollama-status-mode models)
                           (message "Models refreshed successfully")
                           (when callback
                             (funcall callback))))
                       :error (lambda (err)
                                (message "Failed to refresh models: %s" err))))

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
  "Delete the model at point."
  (interactive)
  (let ((model-name (ollama-status--get-model-at-point)))
    (when (and model-name (yes-or-no-p (format "Delete model %s? " model-name)))
      (ollama-delete-model model-name)
      ;; Wait a moment before refresh to ensure deletion completes
      (run-at-time 1 nil
                   (lambda ()
                     (ollama-status-refresh
                      (lambda ()
                        (message "Model %s deleted successfully" model-name))))))))


(defun ollama-show-model-info ()
  "Show detailed information about the model at point."
  (interactive)
  (let ((model-name (ollama-status--get-model-at-point)))
    (when model-name
      (ollama-show-model model-name)
      (pop-to-buffer "*Ollama Model Info*"))))

;;;###autoload
(defun ollama-status ()
  "Show Ollama status in a dedicated buffer."
  (interactive)
  (if (get-buffer ollama-status-buffer-name)
      (pop-to-buffer ollama-status-buffer-name)
    (ollama-status-refresh)))

(provide 'ollama-status)
;;; ollama-status.el ends here
