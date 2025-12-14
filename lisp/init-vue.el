;;; init-vue.el --- Vue 3 / Volar configuration -*- lexical-binding: t -*-

(use-package web-mode
  :ensure t)

(define-derived-mode vue-web-mode web-mode "Vue"
  "Major mode for Vue 3 Single File Components."
  (setq-local web-mode-content-type "vue")
  (setq-local web-mode-markup-indent-offset 2)
  (setq-local web-mode-code-indent-offset 2)
  (setq-local web-mode-css-indent-offset 2)
  (setq-local indent-tabs-mode nil))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-web-mode))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(defun my/vue-lsp-root ()
  "Pick the closest Vue/TS project root (monorepo-safe)."
  (or (locate-dominating-file default-directory "tsconfig.json")
      (locate-dominating-file default-directory "jsconfig.json")
      (locate-dominating-file default-directory "package.json")
      (locate-dominating-file default-directory ".git")
      default-directory))

(with-eval-after-load 'lsp-mode
  ;; Identify this major mode as Vue for LSP.
  (add-to-list 'lsp-language-id-configuration '(vue-web-mode . "vue")))

(defun my/vue-collapse-blank-lines ()
  "Collapse runs of 3+ blank lines down to a single blank line."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n\\{3,\\}" nil t)
      (replace-match "\n\n"))))

(defvar my/vue-lsp-format-timeout 3
  "Seconds to wait for LSP formatting during save. Keep low to avoid freezes.")

(defun my/vue-lsp-format-buffer-safe ()
  "Run `lsp-format-buffer' if available, with a short timeout to avoid freezing."
  (when (and (bound-and-true-p lsp-mode)
             (fboundp 'lsp-format-buffer)
             (fboundp 'lsp-feature?)
             (lsp-feature? "textDocument/formatting"))
    (let ((lsp-response-timeout my/vue-lsp-format-timeout))
      (ignore-errors
        (lsp-format-buffer)))))

(defun my/vue-before-save ()
  "Vue save hook: clean blank lines, then LSP format (best effort)."
  (when (derived-mode-p 'vue-web-mode)
    (my/vue-collapse-blank-lines)
    (my/vue-lsp-format-buffer-safe)))

(defun my/vue-start-lsp ()
  "Start Volar + TS-LS for Vue buffers."
  (setq-local lsp-project-root-function #'my/vue-lsp-root)
  ;; Keep your working clients unchanged
  (setq-local lsp-enabled-clients '(vue-semantic-server ts-ls eslint))
  (lsp-deferred))

(add-hook 'vue-web-mode-hook #'my/vue-start-lsp)

;; Enable save formatting only after LSP actually manages the buffer.
(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'vue-web-mode)
              (add-hook 'before-save-hook #'my/vue-before-save nil t))))

(provide 'init-vue)
;;; init-vue.el ends here
