;;; init-java.el --- Java support (LSP jdtls + format-on-save + compile/run) -*- lexical-binding: t; -*-

(require 'compile)
(require 'cl-lib)

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
;; Project root detection
;; -------------------------

(defun my/java-project-root ()
  "Detect Java project root (Maven/Gradle/Git/monorepo-safe)."
  (or (locate-dominating-file default-directory "pom.xml")
      (locate-dominating-file default-directory "build.gradle")
      (locate-dominating-file default-directory "build.gradle.kts")
      (locate-dominating-file default-directory "settings.gradle")
      (locate-dominating-file default-directory "settings.gradle.kts")
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun my/java--abs-root ()
  "Return expanded absolute project root."
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
        (expand-file-name ".lsp-java-workspace/" user-emacs-directory)
        lsp-java-project-source-paths ["src"]
        lsp-java-maven-download-sources t
        lsp-java-import-maven-enabled t
        lsp-java-import-gradle-enabled t
        lsp-java-format-enabled t
        lsp-java-autobuild-enabled t))

;; -------------------------
;; Format on save
;; -------------------------

(defvar my/java-format-timeout 3
  "Seconds to wait for LSP formatting during save.")

(defun my/java-format-buffer-safe ()
  "Best-effort LSP formatting."
  (when (and (bound-and-true-p lsp-mode)
             (fboundp 'lsp-format-buffer))
    (let ((lsp-response-timeout my/java-format-timeout))
      (ignore-errors (lsp-format-buffer)))))

(defun my/java-before-save ()
  "Java save hook."
  (when (derived-mode-p 'java-mode 'java-ts-mode)
    (my/java-format-buffer-safe)))

;; -------------------------
;; Main class discovery
;; -------------------------

(defvar my/java-main-class-cache (make-hash-table :test #'equal)
  "Cache of selected main classes per project root.")

(defun my/java--project-key ()
  (my/java--abs-root))

(defun my/java--file-to-class (root file)
  "Convert FILE path under ROOT/src to fully qualified Java class."
  (let* ((src-root (expand-file-name "src/" root))
         (abs (expand-file-name file)))
    (when (string-prefix-p src-root abs)
      (let ((rel (file-relative-name abs src-root)))
        (when (string-match "\\(.*\\)\\.java$" rel)
          (replace-regexp-in-string "/" "." (match-string 1 rel)))))))

(defun my/java--find-main-classes ()
  "Return list of fully qualified Java classes with main methods."
  (let* ((root (my/java--abs-root))
         (default-directory root)
         (cmd "grep -R \"public static void main\" -n src"))
    (with-temp-buffer
      (when (eq 0 (call-process-shell-command cmd nil t))
        (goto-char (point-min))
        (let (results)
          (while (re-search-forward "^\\([^:]+\\):" nil t)
            (let ((class (my/java--file-to-class root (match-string 1))))
              (when class
                (push class results))))
          (sort (delete-dups results) #'string<))))))

(defun my/java-select-main-class ()
  "Prompt user to select a Java main class and cache it."
  (interactive)
  (let* ((mains (my/java--find-main-classes))
         (choice (completing-read "Run main class: " mains nil t)))
    (puthash (my/java--project-key) choice my/java-main-class-cache)
    choice))

(defun my/java--get-main-class ()
  "Return cached main class or prompt if missing."
  (or (gethash (my/java--project-key) my/java-main-class-cache)
      (my/java-select-main-class)))

;; -------------------------
;; Compile + Run
;; -------------------------

(defun my/java--compile-command ()
  "Compile Java sources and run selected main class."
  (let ((root (my/java--abs-root))
        (main (my/java--get-main-class)))
    (format
     "cd %s && mkdir -p out && javac -d out $(find src -name '*.java') && java -cp out %s"
     (shell-quote-argument root)
     main)))

(defun my/java-run ()
  "Compile and run Java with selectable main class."
  (interactive)
  (compile (my/java--compile-command)))

(defun my/compile-ask ()
  "Always prompt for compile command."
  (interactive)
  (let ((compilation-read-command t))
    (call-interactively #'compile)))

;; -------------------------
;; Java buffer setup
;; -------------------------

(defun my/java-mode-setup ()
  "Setup Java buffers."
  (setq-local lsp-project-root-function #'my/java-project-root)
  (lsp-deferred)

  ;; format-on-save
  (add-hook 'before-save-hook #'my/java-before-save nil t)

  ;; organize imports
  (local-set-key (kbd "C-c i") #'lsp-organize-imports)

  ;; make M-x compile run Java
  (setq-local compile-command (my/java--compile-command)))

(add-hook 'java-mode-hook #'my/java-mode-setup)
(with-eval-after-load 'java-ts-mode
  (add-hook 'java-ts-mode-hook #'my/java-mode-setup))

;; -------------------------
;; Keybindings
;; -------------------------

(global-set-key (kbd "C-c j r") #'my/java-run)      ;; compile + run
(global-set-key (kbd "C-c j R") #'recompile)        ;; rerun
(global-set-key (kbd "C-c j c") #'my/compile-ask)   ;; prompt
(global-set-key (kbd "C-c j f") #'my/java-format-buffer-safe)

(provide 'init-java)
;;; init-java.el ends here
