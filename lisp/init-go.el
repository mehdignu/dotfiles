;;; init-go.el --- Go development configuration -*- lexical-binding: t -*-

;; Silence compiler warnings
(defvar lsp-go-use-gofumpt)
(defvar lsp-go-build-flags)
(defvar lsp-completion-enable-additional-text-edit)
(defvar lsp-go-import-on-save)
(defvar lsp-go-analyses)
(defvar lsp-go-codelenses)

(declare-function lsp-organize-imports "lsp-mode")
(declare-function lsp-format-buffer "lsp-mode")

;; -------------------------
;; Go Mode + LSP
;; -------------------------

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred))

(with-eval-after-load 'lsp-mode
  (setq lsp-go-use-gofumpt t
        lsp-go-build-flags ["-tags=integration"]
        lsp-completion-enable-additional-text-edit t
        lsp-go-import-on-save t
        lsp-go-analyses '((unusedparams . t)
                          (shadow . t))
        lsp-go-codelenses '((gc_details . t)
                            (generate . t)
                            (regenerate_cgo . t)
                            (tidy . t)
                            (upgrade_dependency . t)
                            (vendor . t))))

;; -------------------------
;; Go Keybindings
;; -------------------------

(add-hook 'go-mode-hook
          (lambda ()
            ;; organize imports
            (local-set-key (kbd "C-c i") #'lsp-organize-imports)

            ;; format on save
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

;; Disable old godef binding
(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c C-d") nil))

;; -------------------------
;; Protobuf (.proto) Support
;; -------------------------

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-mode))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(protobuf-mode . "proto"))
  (when (executable-find "buf")
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("buf" "lsp" "serve"))
      :major-modes '(protobuf-mode)
      :server-id 'buf-lsp))))

(add-hook 'protobuf-mode-hook #'lsp-deferred)

(defun my/proto-format-on-save ()
  "Format current .proto buffer using buf format, if buf is installed."
  (when (and buffer-file-name
             (string-match-p "\\.proto\\'" buffer-file-name)
             (executable-find "buf"))
    (call-process "buf" nil "*buf-format*" nil "format" "-w" buffer-file-name)
    (revert-buffer :ignore-auto :noconfirm)))

(add-hook 'protobuf-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c i") #'lsp-organize-imports)
            (add-hook 'before-save-hook #'my/proto-format-on-save nil t)))

;; -------------------------
;; Go Project Commands
;; -------------------------

(defun go-run ()
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((default-directory (file-name-directory buffer-file-name)))
    (compile "go run .")))

(defun go-test ()
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((default-directory (file-name-directory buffer-file-name)))
    (compile "go test ./...")))

(defun go-build ()
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((default-directory (file-name-directory buffer-file-name)))
    (compile "go build ./...")))

(global-set-key (kbd "C-c r") #'go-run)
(global-set-key (kbd "C-c b") #'go-build)

(provide 'init-go)
