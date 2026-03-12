;; .emacs

;; Ghostty terminal support - must come very early
(when (getenv "GHOSTTY_RESOURCES_DIR")
  ;; Ensure proper terminal detection
  (setq-default system-uses-terminfo t)
  ;; Prevent vterm compilation issues in Ghostty
  (setq vterm-always-compile-module nil)

  ;; Filter out harmless nil face attribute warnings
  ;; The leuven theme and some packages set faces with nil values which Ghostty doesn't like
  (defun ghostty-filter-face-warnings (orig-fun type message &optional level buffer-name)
    "Filter out harmless nil face attribute warnings."
    (unless (and (eq type 'initialization)
                 (stringp message)
                 (string-match-p "setting attribute.*nil value" message))
      (funcall orig-fun type message level buffer-name)))

  (advice-add 'display-warning :around #'ghostty-filter-face-warnings))

;; Use hunspell for spelling
(setq ispell-program-name "hunspell")

;; Full spell checking for text
(add-hook 'text-mode-hook #'flyspell-mode)
;; Comments & strings only in code
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; handle issues with backups
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("cselpa" . "https://elpa.thecybershadow.net/packages/"))
(setq package-check-signature nil)

;; display column number
(setq column-number-mode t)

;; compilation-mode scrolls to bottom
(setq compilation-scroll-output t)

;; turn on and off global whitespace mode
(global-whitespace-mode 0)
(global-set-key (kbd "C-c 0") 'global-whitespace-mode)

;; replace-string keybind
(global-set-key (kbd "C-c s") 'replace-string)

(package-initialize)

;; Ensure use-package is installed and loaded
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure nil)  ;; Don't auto-install unless :ensure t is specified

;; Enable :vc keyword support in use-package (Emacs 29+)
;; This integrates package-vc with use-package
(when (>= emacs-major-version 29)
  (require 'package-vc)
  (add-to-list 'use-package-keywords :vc t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(idris2-stay-in-current-window-on-compiler-error t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(clang-format lsp-mode yasnippet lsp-treemacs helm-lsp projectile
		  hydra flycheck company avy which-key helm-xref
		  dap-mode))
 '(package-vc-selected-packages
   '((lean4-mode :url
		 "https://github.com/leanprover-community/lean4-mode.git")
     (monet :url "https://github.com/stevemolitor/monet")
     (claude-code :url
		  "https://github.com/stevemolitor/claude-code.el"))))

;; hack to get around weird encoding issue from ghostty sending
;; the wrong thing for C-c C-i
(with-eval-after-load 'lean4-mode
     (define-key lean4-mode-map (kbd "C-c C-j") #'lean4-toggle-info))

(setq mac-option-modifier 'meta) ; set alt-key to meta
(setq mac-escape-modifier nil) ; set esc-key to nil

;; Install magit and dependencies from GitHub to avoid version mismatches
(use-package compat :ensure t :defer t)

(use-package with-editor
  :defer t
  :vc (:url "https://github.com/magit/with-editor" :rev :newest)
  :ensure t)

(use-package transient
  :defer t
  :vc (:url "https://github.com/magit/transient" :rev :newest)
  :ensure t)

(use-package magit
  :defer t
  :vc (:url "https://github.com/magit/magit" :rev :newest)
  :ensure t)


;;; uncomment for CJK utf-8 support for non-Asian users
;; (require 'un-define)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(proof-locked-face ((t (:extend t :background "cornsilk" :foreground "brightblue" :underline nil)))))


(setq overlay-arrow-string "")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'leuven t)


;; save on loss of focus
(defun save-all ()
    (interactive)
    (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

;; turn on company-mode for programming envs
(add-hook 'prog-mode-hook 'company-mode)


;; hungry-delete commands
(global-set-key (kbd "C-c DEL") 'hungry-delete-backward)
(global-set-key (kbd "C-c <deletechar>") 'hungry-delete-forward)


;; multiple cursors set up
(require 'multiple-cursors)
;;(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "C-c e") 'mc/edit-lines)


;; add key to run makefile in project
(global-set-key (kbd "C-c m") 'recompile)

;; turn on linum in all programming environments
(add-hook 'prog-mode-hook 'line-number-mode)

;; python-mode options
(setq-default python-indent-offset 4)


;; run clang-format on save
(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
              (when (locate-dominating-file "." ".clang-format")
                (clang-format-buffer))
              ;; Continue to save.
              nil)
            nil
            ;; Buffer local hook.
            t))

(defun add-clang-format-hook ()
  (clang-format-save-hook-for-this-buffer))

(add-hook 'c-mode-hook #'add-clang-format-hook)
(add-hook 'c++-mode-hook #'add-clang-format-hook)
(add-hook 'glsl-mode-hook #'add-clang-format-hook)


;;; LSP Configuration

;; Install LSP and related packages if needed
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

;; idris2
(add-to-list 'load-path "~/.emacs.d/idris2-mode")
(require 'idris2-mode)
(setq company-global-modes '(not idris2-mode))

;; straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
       (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; lean4
(use-package lean4-mode
  :commands lean4-mode
  :vc (:url "https://github.com/leanprover-community/lean4-mode.git"
       :rev :last-release
       ;; Or, if you prefer the bleeding edge version of Lean4-Mode:
       ;; :rev :newest
       ))


;; agda keybindings as mac terminal is useless
(eval-after-load 'agda2-mode
  '(progn
     (define-key agda2-mode-map (kbd "C-c C-u") #'agda2-goal-and-context)
     (define-key agda2-mode-map (kbd "C-u C-u C-c C-u") #'agda2-goal-and-context)))


;; Add ability to call loogle when in lean4 mode
(defun run-loogle-with-input-async ()
  "Prompt for a string, call the `loogle` CLI tool with the string wrapped in single quotes asynchronously, and show results in a buffer."
  (interactive)
  (let* ((input (read-string "Enter search string: "))
         (command (format "loogle '%s'" input))
         (buffer-name "*Loogle Results*"))
    ;; Use async-shell-command to avoid blocking Emacs
    (async-shell-command command buffer-name)
    ;; Switch buffer to compilation-mode for clickable output
    (with-current-buffer buffer-name
      (compilation-mode))))


;; Add key binding only in lean4-mode
(with-eval-after-load 'lean4-mode
  (define-key lean4-mode-map (kbd "C-c l") #'run-loogle-with-input-async))


(message "Loading agda-mode...")
(condition-case err
    (load-file (let ((coding-system-for-read 'utf-8))
                    (shell-command-to-string "agda --emacs-mode locate")))
  (error (message "Failed to load agda-mode: %s" err)))
(message "Agda-mode loaded")

;; remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Add claude-code
(use-package monet
  :defer t
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

;; install required inheritenv dependency:
(use-package inheritenv
  :defer t
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

;; for vterm terminal backend:
(use-package vterm
  :ensure t
  :defer t
  :init
  ;; Prevent vterm from compiling module during startup
  (setq vterm-always-compile-module nil))

;; install claude-code.el
(use-package claude-code :ensure t
  :defer t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :init
  ;; Load claude-code lazily after startup
  (run-with-idle-timer 2 nil
    (lambda ()
      (require 'claude-code)
      (require 'monet)
      ;; optional IDE integration with Monet
      (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
      (monet-mode 1)
      (claude-code-mode)))
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

;; standardml set up
(use-package sml-mode
  :defer t
  :mode ("\\.\\(sml\\|sig\\)\\'" . sml-mode))

(use-package eglot
  :ensure t
  :hook ((sml-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '((sml-mode) "millet-ls")))
