;;; init-python.el --- Python development configuration

;; -------------------------
;; Python Major Mode
;; -------------------------

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :hook ((python-mode . lsp-deferred)
         (python-mode . my/python-auto-activate-venv)
         (python-mode . my/python-ruff-format-on-save))
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  (python-shell-interpreter-args "-i"))

;; -------------------------
;; Virtual Environment (.venv)
;; -------------------------

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

(defun my/python-auto-activate-venv ()
  "Automatically activate .venv in the Projectile project root."
  (when-let ((root (projectile-project-root)))
    (let ((venv (expand-file-name ".venv" root)))
      (when (file-directory-p venv)
        (pyvenv-activate venv)))))

;; -------------------------
;; LSP: Pyright
;; -------------------------

(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :custom
  (lsp-pyright-typechecking-mode "basic")
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-use-library-code-for-types t))

;; -------------------------
;; Ruff: Formatting on Save
;; -------------------------

(defun my/python-ruff-format-buffer ()
  "Format the current buffer using ruff format."
  (when (and buffer-file-name
             (eq major-mode 'python-mode))
    (call-process "ruff" nil "*ruff-format*" nil
                  "format" buffer-file-name)
    (revert-buffer t t t)))

(defun my/python-ruff-format-on-save ()
  "Add Ruff formatting before save."
  (add-hook 'before-save-hook #'my/python-ruff-format-buffer nil t))

;; -------------------------
;; Flycheck: Ruff Linting
;; -------------------------

(use-package flycheck
  :ensure t)

(with-eval-after-load 'flycheck
  (flycheck-define-checker python-ruff
    "A Python linter using Ruff."
    :command ("ruff" "check" "--output-format=text" source)
    :error-patterns
    ((error line-start
            (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            " "
            (message (one-or-more not-newline))
            line-end))
    :modes python-mode)

  (add-to-list 'flycheck-checkers 'python-ruff))

;; -------------------------
;; pytest Integration
;; -------------------------

(use-package pytest
  :ensure t
  :after python
  :config
  (setq pytest-cmd-flags "-q"))

;; Keybindings
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-t a") #'pytest-all)
  (define-key python-mode-map (kbd "C-c C-t f") #'pytest-one)
  (define-key python-mode-map (kbd "C-c C-t t") #'pytest-pdb-one)
  (define-key python-mode-map (kbd "C-c C-r") #'python-shell-send-region)
  (define-key python-mode-map (kbd "C-c C-b") #'python-shell-send-buffer))

;; -------------------------
;; Debugging (DAP)
;; -------------------------

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

(provide 'init-python)
;;; init-python.el ends here
