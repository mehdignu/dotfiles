;; -------------------------
;; Package Setup
;; -------------------------
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))


;; -------------------------
;; Go Development Setup
;; -------------------------

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

(use-package lsp-mode
  :ensure t
  :hook (go-mode . lsp-deferred)
  :init
  (setq lsp-prefer-flymake nil)         ;; use Flycheck instead of Flymake
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-diagnostic-package :flycheck)
  (setq lsp-idle-delay 0.2))            ;; real-time diagnostics (200ms)

(use-package flycheck
  :ensure t
  :hook (go-mode . flycheck-mode))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-doc-enable t))


;; -------------------------
;; Autocompletion
;; -------------------------

(use-package company
  :ensure t
  :hook (go-mode . company-mode)
  :config
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1))


;; -------------------------
;; Quality of Life
;; -------------------------

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))


;; -------------------------
;; Custom
;; -------------------------

(custom-set-variables
 '(package-selected-packages nil))

(custom-set-faces)
