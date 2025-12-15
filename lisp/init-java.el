;;; init-java.el --- Java support (LSP jdtls + format-on-save + compile/recompile) -*- lexical-binding: t; -*-

(require 'compile)

;; Silence compiler warnings (vars are defined by lsp-java)
(defvar lsp-java-workspace-dir)
(defvar lsp-java-maven-download-sources)
(defvar lsp-java-import-gradle-enabled)
(defvar lsp-java-import-maven-enabled)
(defvar lsp-java-format-enabled)
(defvar lsp-java-autobuild-enabled)

(declare-function lsp-deferred "lsp-mode")
(declare-function lsp-format-buffer "lsp-mode")
(declare-function lsp-organize-imports "lsp-mode")

;; -------------------------
;; Project root detection (monorepo-safe)
;; -------------------------

(defun my/java-project-root ()
  "Pick a reasonable Java project root (Maven/Gradle/monorepo-safe)."
  (or (locate-dominating-file default-directory "pom.xml")
      (locate-dominating-file default-directory "build.gradle")
      (locate-dominating-file default-directory "build.gradle.kts")
      (locate-dominating-file default-directory "settings.gradle")
      (locate-dominating-file default-directory "settings.gradle.kts")
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun my/java--abs-root ()
  "Return absolute (expanded) project root directory."
  (file-name-as-directory (expand-file-name (my/java-project-root))))

;; -------------------------
;; LSP (jdtls via lsp-java)
;; -------------------------

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred))

(use-package lsp-java
  :ensure t
  :after lsp-mode
  :init
  (setq lsp-java-workspace-dir
        (expand-file-name ".lsp-java-workspace/" user-emacs-directory))
  (setq lsp-java-maven-download-sources t
        lsp-java-import-maven-enabled t
        lsp-java-import-gradle-enabled t
        lsp-java-format-enabled t
        lsp-java-autobuild-enabled t))

;; -------------------------
;; Format on save (Java)
;; -------------------------

(defvar my/java-format-timeout 3
  "Seconds to wait for LSP formatting during save.")

(defun my/java-format-buffer-safe ()
  "Format current buffer via LSP best-effort."
  (when (and (bound-and-true-p lsp-mode)
             (fboundp 'lsp-format-buffer))
    (let ((lsp-response-timeout my/java-format-timeout))
      (ignore-errors (lsp-format-buffer)))))

(defun my/java-before-save ()
  "Java save hook."
  (when (derived-mode-p 'java-mode 'java-ts-mode)
    (my/java-format-buffer-safe)))

;; -------------------------
;; Compile + Run (for M-x compile / recompile)
;; -------------------------

(defvar my/java-main-class "LinkedList.Main"
  "Fully qualified main class to run (e.g. \"com.example.Main\").")

(defun my/java--compile-command ()
  "Compile all Java sources under src/ into out/, then run `my/java-main-class`."
  (let ((root (my/java--abs-root)))
    ;; Expand ~ first, then quote for shell safety (spaces etc.)
    (format "cd %s && mkdir -p out && javac -d out $(find src -name '*.java') && java -cp out %s"
            (shell-quote-argument root)
            my/java-main-class)))

(defun my/java-run ()
  "Compile + run using `compile` (gives clickable errors)."
  (interactive)
  (compile (my/java--compile-command)))

(defun my/compile-ask ()
  "Run `compile`, always prompting for the command."
  (interactive)
  (let ((compilation-read-command t))
    (call-interactively #'compile)))

(global-set-key (kbd "C-c j c") #'my/compile-ask)


;; -------------------------
;; Java buffer setup
;; -------------------------

(defun my/java-mode-setup ()
  "Setup Java buffers: LSP, format-on-save, compile-command, keybindings."
  (setq-local lsp-project-root-function #'my/java-project-root)
  (lsp-deferred)

  ;; Buffer-local format-on-save
  (add-hook 'before-save-hook #'my/java-before-save nil t)

  ;; Organize imports
  (local-set-key (kbd "C-c i") #'lsp-organize-imports)

  ;; Make M-x compile / M-x recompile run Java instead of make
  (setq-local compile-command (my/java--compile-command)))

(add-hook 'java-mode-hook #'my/java-mode-setup)
(with-eval-after-load 'java-ts-mode
  (add-hook 'java-ts-mode-hook #'my/java-mode-setup))

;; -------------------------
;; Keybindings
;; -------------------------

(global-set-key (kbd "C-c j r") #'my/java-run)          ;; run now
(global-set-key (kbd "C-c j R") #'recompile)            ;; rerun last compile
(global-set-key (kbd "C-c j f") #'my/java-format-buffer-safe) ;; manual format (debug)

(provide 'init-java)
;;; init-java.el ends here
