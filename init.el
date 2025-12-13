;; -------------------------
;; Package Setup
;; -------------------------
(require 'package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(require 'init-go)

;; -------------------------
;; YAML Editing Support
;; -------------------------

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))


;; -------------------------
;; YAML LSP Support
;; -------------------------

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(yaml-mode . "yaml"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("yaml-language-server" "--stdio"))
    :major-modes '(yaml-mode)
    :activation-fn (lsp-activate-on "yaml")
    :server-id 'yaml-ls)))
;; Kubernetes + Docker YAML Validation
(with-eval-after-load 'lsp-yaml
  (setq lsp-yaml-schemas
        '((kubernetes . "/*.k8s.yaml")
          ("https://json.schemastore.org/github-workflow.json" . "/.github/workflows/*"))))

  ;; LSP documentation popup
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-ui-doc-delay 0.2)

(with-eval-after-load 'lsp-mode
  ;; hover docs
  (define-key lsp-mode-map (kbd "C-c d") 'lsp-ui-doc-glance)
  ;; detailed docs
  (define-key lsp-mode-map (kbd "C-h .") 'lsp-describe-thing-at-point))

;; Snippet support (required by LSP for many completions)
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-doc-enable t))

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


;; move better betwen windows
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)



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



(pixel-scroll-precision-mode 1)

(setq scroll-margin 3
      scroll-step 1
      scroll-conservatively 101
      mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(1))

;; Add Real Project Awareness
(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/dev/" "~/src/"))
  (setq projectile-completion-system 'auto)
  :config
  (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Add a Better Search Experience (ripgrep + consult)
(use-package consult
  :ensure t)

(use-package consult-projectile
  :after (consult projectile)
  :ensure t)


;; Install ripgrep externally: brew install ripgrep OR apt install ripgrep
(global-set-key (kbd "C-c s") #'consult-ripgrep)

;; Treemacs for Project Browsing
(use-package treemacs
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(global-set-key (kbd "C-c e") #'treemacs) ;; e = explorer


;; Add Smart Buffer + File Switching (Vertico + Orderless)
(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless)))

;; Improve LSP Performance for Big Projects
(setq lsp-log-io nil)
(setq lsp-completion-provider :none) ;; let company handle it
(setq read-process-output-max (* 1024 1024)) ;; 1MB



(use-package company
  :init (global-company-mode))

(use-package flycheck
  :init (global-flycheck-mode))
