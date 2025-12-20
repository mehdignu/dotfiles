;;; init-org.el --- Org Mode configuration -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :mode (("\\.org\\'" . org-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :custom
  ;; Basic editing / display
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-ellipsis " ▾")
  (org-pretty-entities t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300))

  ;; Files 
  (org-directory (expand-file-name "~/org/"))
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-agenda-files (list org-directory))

  ;; Logging / workflow
  (org-log-done 'time)
  (org-log-into-drawer t)

  ;; Refile targets
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))

  :config
  ;; Ensure org-directory exists
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))

  ;; Capture templates (minimal, practical defaults)
  (setq org-capture-templates
        `(("t" "Todo" entry
           (file+headline ,(expand-file-name "inbox.org" org-directory) "Tasks")
           "* TODO %?\n  %U\n  %a\n")
          ("n" "Note" entry
           (file+headline ,(expand-file-name "inbox.org" org-directory) "Notes")
           "* %?\n  %U\n  %a\n"))))

(provide 'init-org)
;;; init-org.el ends here
