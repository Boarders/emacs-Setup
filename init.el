(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups
;; save on loss of focus
(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 0)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 10)))
(package-initialize)

; update packages list if we are on a new install
(unless package-archive-contents
  (package-refresh-contents))

;; turn off the damn bell
(setq visible-bell 1)

;; a list of pkgs to programmatically install
;; ensure installed via package.el
(setq my-package-list '(use-package))

;; programmatically install/ensure installed
;; pkgs in your personal list
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; remove cl warning
(setq byte-compile-warnings '(cl-functions))

;; add custom themes folder
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'solarized-dark t)

(defun save-all ()
    (interactive)
    (save-some-buffers t))
  (add-hook 'focus-out-hook 'save-all)
(setq column-number-mode t)
(add-hook 'prog-mode-hook 'linum-mode) ; turn on line number mode in all programming environments
(global-whitespace-mode 0)
(global-set-key (kbd "C-c C-0") 'global-whitespace-mode)
(setq whitespace-line-column 200)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default python-indent-offset 4)
(setq indent-line-function 'insert-tab)
;; add key to run makefile in project
(global-set-key (kbd "C-c m") 'recompile)
(global-set-key (kbd "C-c s") 'replace-string)
(global-unset-key (kbd "C-i"))
(global-set-key (kbd "<C-return>") 'newline)
(global-unset-key (kbd "C-x C-u"))
(require 'package)
(setq package-check-signature 'nil)
;; make whitespace-mode use just basic coloring
;;(set-face-attribute 'whitespace-space nil :background nil :foreground "gray30")
(setq whitespace-display-mappings
  ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
   '(
      (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
      (newline-mark 10 [8617 10]) ; 10 LINE FEED
      (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
    )
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(fci-rule-color "#073642")
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type 'cabal-new-repl)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   '(zig-mode projectile fish-mode flycheck-ats2 elm-mode purescript-mode lsp-treemacs flyspell-correct helm-lean company-lean lean-mode lsp-haskell lsp-ui lsp-mode yasnippet dap-mode magit proof-general god-mode psc-ide racket-mode hungry-delete solarized-theme multiple-cursors merlin tuareg use-package dante rust-mode idris-mode haskell-mode))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 0)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 10)))
;;(package-initialize)
'(package-selected-packages (quote (haskell-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-bound-variable-face ((t (:foreground "Orange"))))
 '(agda2-highlight-datatype-face ((t (:foreground "LightGreen"))))
 '(agda2-highlight-function-face ((t (:foreground "Green"))))
 '(agda2-highlight-inductive-constructor-face ((t (:foreground "Orange"))))
 '(agda2-highlight-keyword-face ((t (:foreground "windowFrameColor"))))
 '(agda2-highlight-module-face ((t (:foreground "White"))))
 '(agda2-highlight-number-face ((t (:foreground "Orange"))))
 '(agda2-highlight-operator-face ((t (:foreground "Green"))))
 '(agda2-highlight-postulate-face ((t (:foreground "Orange"))))
 '(agda2-highlight-primitive-face ((t (:foreground "White"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "LightGreen"))))
 '(agda2-highlight-record-face ((t (:foreground "keyboardFocusIndicatorColor"))))
 '(agda2-highlight-unsolved-constraint-face ((t (:foreground "White" :background: "Red"))))
 '(agda2-highlight-unsolved-meta-face ((t (:foreground "White" :background: "Orange")))))
(load-file (let ((coding-system-for-read 'utf-8))
         (shell-command-to-string "agda-mode locate")))

;; haskell mode stuff
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-u") "undefined")
  (define-key haskell-mode-map (kbd "C-c C-SPC") `comment-region)
  (define-key haskell-mode-map (kbd "\C-ch") 'haskell-hoogle)
  (setq haskell-hoogle-command "hoogle")))






;; (use-package lsp-ui)
(require 'haskell-mode)
(require 'rust-mode)
(require 'use-package)
(require 'agda2-mode)
(require 'racket-mode)


(defun remove-electric-indent-mode ()
  "Turn off electric indent mode locally."
  (interactive)
  (electric-indent-local-mode -1))



(global-set-key (kbd "C-c e") 'electric-indent-local-mode)


(add-hook 'agda2-mode-hook
  (lambda ()
    (remove-electric-indent-mode)
    (define-key agda2-mode-map (kbd "C-i") nil)))
;;    (define-key agda2-mode-map (kbd "RET") 'newline-and-indent)))

(add-hook 'python-mode-hook
  (lambda ()
    (define-key python-mode-map (kbd "RET") 'newline-and-indent)))

(eval-after-load 'agda2-mode
  '(progn
     (remove-electric-indent-mode)
   )
)


;; multiple cursors set up
(require 'multiple-cursors)
(global-set-key (kbd "C-i") nil)
(global-set-key (kbd "C-i C-i") 'mc/edit-lines)


;; hungry-delete commands
(global-set-key (kbd "C-c <backspace>") 'hungry-delete-backward)
(global-set-key (kbd "C-c <deletechar>") 'hungry-delete-forward)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; purescript ide
(require 'psc-ide)

(add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (turn-on-purescript-indentation)))

(add-to-list 'auto-mode-alist '("\\.purs\\'" . purescript-mode))


;; sblc slime
(load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "/usr/bin/sbcl")


;; company completion key
(global-set-key (kbd "M-\"") #'company-complete)

;; ats setup
(add-to-list 'load-path "~/.emacs.d/ats-mode")
(setenv "PATSHOME" "/usr/local/bin/patscc")

(require 'ats2-flymake)
(require 'ats2-flycheck)
(require 'ats-mode)
(add-hook 'ats-mode-hook
 (lambda ()
   (ats2-flycheck)
   (ats2-flymake)
   (ats2-mode)))

(add-to-list 'auto-mode-alist '("\\.dats\\'" . ats-mode))

(provide 'init)
;;; Init.el ends here


;; ghcid
(defun ghcid ()
  "Runs ghcid in a `term' buffer."
  (interactive)
  (require 'term)
  (let* ((cmd "ghcid")
         (h (- (window-height) scroll-margin 3))
         (term-buffer-maximum-size h)
         (args
          (mapconcat 'identity
            (list (format "-h %d" h) (format "-c \"cabal repl -fno-code\"")) " "))
         (switches (split-string-and-unquote args))
         (termbuf (apply 'make-term "ghcid" cmd nil switches)))
    (set-buffer termbuf)
    (term-mode)
    (term-char-mode)
    (compilation-minor-mode)
    (switch-to-buffer termbuf)))

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; ido-mode
(require 'ido)
(ido-mode t)
(ido-everywhere t)

;; 2018-10-10, jsled: from https://www.reddit.com/r/emacs/comments/9mvdpg/i_dont_want_emacs_to_ever_create_a_new_frame/
(define-advice make-frame (:around (fn &rest args) suppress)
  "Suppress making new frame; return existing frame."
  (message "make-frame suppressed; proceed at your own peril.")
  (selected-frame))
;; ediff set up to not use mutltiple frames
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
