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
  (setq lsp-prefer-flymake nil)
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-diagnostic-package :flycheck)
  (setq lsp-idle-delay 0.2)
  (setq lsp-go-use-gofumpt t)
  (setq lsp-go-build-flags ["-tags=integration"])
  ;; auto-import support
  (setq lsp-completion-enable-additional-text-edit t)
  (setq lsp-go-import-on-save t)
  :config
  (setq lsp-go-analyses
        '((unusedparams . t)
          (shadow . t)))
  (setq lsp-go-codelenses
        '((gc_details . t)
          (generate . t)
          (regenerate_cgo . t)
          (tidy . t)
          (upgrade_dependency . t)
          (vendor . t))))

;; Manual import control keybinding
(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c i") #'lsp-organize-imports))

  ;; LSP documentation popup
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-ui-doc-delay 0.2)

(with-eval-after-load 'lsp-mode
  ;; hover docs
  (define-key lsp-mode-map (kbd "C-c d") 'lsp-ui-doc-glance)
  ;; detailed docs
  (define-key lsp-mode-map (kbd "C-h .") 'lsp-describe-thing-at-point))

;; disable old go-mode doc binding that requires godef
(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c C-d") nil))



;; Snippet support (required by LSP for many completions)
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

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


;; git with magit
(use-package magit
  :ensure t
  :commands (magit-status)
  :bind (("C-x g" . magit-status)))



;; -------------------------
;; Custom
;; -------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



