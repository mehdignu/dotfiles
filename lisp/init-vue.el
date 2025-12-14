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

(defun my/vue-start-lsp ()
  "Start Volar + TS-LS for Vue buffers."
  (setq-local lsp-project-root-function #'my/vue-lsp-root)
  ;; IMPORTANT: Volar expects to use ts-ls (default lsp-volar-typescript-server-id is ts-ls).
  (setq-local lsp-enabled-clients '(vue-semantic-server ts-ls eslint))
  (lsp-deferred))

(add-hook 'vue-web-mode-hook #'my/vue-start-lsp)

(provide 'init-vue)
;;; init-vue.el ends here
