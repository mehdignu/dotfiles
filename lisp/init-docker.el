;;; init-docker.el --- Docker + Docker Compose configuration

;; -------------------------
;; Dockerfile Editing
;; -------------------------

(use-package dockerfile-mode
  :ensure t
  :mode (("Dockerfile\\'" . dockerfile-mode)
         ("\\.dockerfile\\'" . dockerfile-mode))
  :hook (dockerfile-mode . lsp-deferred))

;; -------------------------
;; Docker CLI UI
;; -------------------------

(use-package docker
  :ensure t
  :bind (("C-c D" . docker)))

;; -------------------------
;; Docker Compose
;; -------------------------

(use-package docker-compose-mode
  :ensure t
  :mode (("docker-compose\\.ya?ml\\'" . docker-compose-mode)
         ("compose\\.ya?ml\\'" . docker-compose-mode))
  :hook (docker-compose-mode . lsp-deferred))

;; -------------------------
;; Docker Compose Helpers
;; -------------------------

(defun my/docker--project-root ()
  (or (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
      default-directory))

(defun my/docker-compose-up ()
  (interactive)
  (let ((default-directory (my/docker--project-root)))
    (compile "docker compose up")))

(defun my/docker-compose-up-build ()
  (interactive)
  (let ((default-directory (my/docker--project-root)))
    (compile "docker compose up --build")))

(defun my/docker-compose-down ()
  (interactive)
  (let ((default-directory (my/docker--project-root)))
    (compile "docker compose down")))

(defun my/docker-compose-logs ()
  (interactive)
  (let ((default-directory (my/docker--project-root)))
    (compile "docker compose logs -f")))

;; -------------------------
;; Docker Key Prefix
;; -------------------------

(define-prefix-command 'my/docker-prefix)
(global-set-key (kbd "C-c d") 'my/docker-prefix)

(define-key my/docker-prefix (kbd "d") #'docker)
(define-key my/docker-prefix (kbd "u") #'my/docker-compose-up)
(define-key my/docker-prefix (kbd "b") #'my/docker-compose-up-build)
(define-key my/docker-prefix (kbd "D") #'my/docker-compose-down)
(define-key my/docker-prefix (kbd "l") #'my/docker-compose-logs)


(provide 'init-docker)
;;; init-docker.el ends here
