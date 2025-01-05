;;; ollama-transient.el --- Transient menu for ollama.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jiale.liu

;; Author: jiale.liu <im@liujiale.me>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (transient "0.3.0") (ollama "0.1"))
;; Keywords: ollama, ai, models, transient
;; URL: https://github.com/nailuoGG/ollama.el

;;; Commentary:
;; This package provides a transient menu interface for ollama.el commands.

;;; Code:

(require 'transient)
(require 'ollama)

;;;###autoload
(transient-define-prefix ollama-transient-menu ()
  "Ollama model management interface."
  ["Manage Models"
   ("l" "List models" ollama-list-models)
   ("p" "Pull model" ollama-pull-model)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun ollama-transient-setup ()
  "Setup ollama transient menu."
  (interactive)
  (ollama-transient-menu))

(provide 'ollama-transient)
;;; ollama-transient.el ends here
