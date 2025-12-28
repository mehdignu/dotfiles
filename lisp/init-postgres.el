;;; init-postgres.el --- PostgreSQL + SQL tooling -*- lexical-binding: t -*-

;; -------------------------
;; Core SQL Support (BUILT-IN)
;; -------------------------

(use-package sql
  :ensure nil
  :config
  (setq sql-product 'postgres)

  (setq sql-postgres-login-params
        '((user :default "")
          (database :default "")
          (server :default "localhost")
          (port :default 5432)))

  (setq sql-input-ring-file-name
        (expand-file-name "sql-history" user-emacs-directory)))

;; -------------------------
;; SQL Indentation
;; -------------------------

(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

;; -------------------------
;; LSP Support for SQL
;; -------------------------

(use-package lsp-mode
  :commands lsp
  :hook (sql-mode . lsp)
  :config
  (setq lsp-sqls-server
        '(:command ("sql-language-server" "up" "--method" "stdio")
          :settings
          (:sqls (:connections [])))))

;; -------------------------
;; Flycheck (PostgreSQL syntax via psql)
;; -------------------------

(with-eval-after-load 'flycheck
  (flycheck-define-checker sql-postgres
    "PostgreSQL syntax checker using psql."
    :command ("psql" "-X" "-q" "-d" "postgres" "-f" source)
    :error-patterns
    ((error line-start "ERROR:  " (message) line-end))
    :modes sql-mode)

  (add-to-list 'flycheck-checkers 'sql-postgres))

(provide 'init-postgres)
;;; init-postgres.el ends here
