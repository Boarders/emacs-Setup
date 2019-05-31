(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups
;; save on loss of focus

;; add custom themes folder
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; add custom library folder
(add-to-list 'load-path "~/.emacs.d/lib/haskell-mode/")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.emacs.d/lib/haskell-mode/")


(defun save-all ()
    (interactive)
    (save-some-buffers t))
  (add-hook 'focus-out-hook 'save-all)
(load-theme 'wombat t) ; load custom theme here
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
(require 'package)
;; make whitespace-mode use just basic coloring
(set-face-attribute 'whitespace-space nil :background nil :foreground "gray30")
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
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
  (quote
   ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#073642")
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-new-repl))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
  (--map
   (solarized-color-blend it "#002b36" 0.25)
   (quote
    ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
  (quote
   (("#073642" . 0)
    ("#546E00" . 20)
    ("#00736F" . 30)
    ("#00629D" . 50)
    ("#7B6000" . 60)
    ("#8B2C02" . 70)
    ("#93115C" . 85)
    ("#073642" . 100))))
 '(hl-bg-colors
  (quote
   ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
  (quote
   ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
  (quote
   (racket-mode hungry-delete solarized-theme multiple-cursors merlin tuareg use-package dante rust-mode idris-mode haskell-mode)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
  (quote
   ((20 . "#dc322f")
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
    (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
  (quote
   (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))
(package-initialize)
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
 '(agda2-highlight-record-face ((t (:foreground "keyboardFocusIndicatorColor")))))
(load-file (let ((coding-system-for-read 'utf-8))
         (shell-command-to-string "agda-mode locate")))

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "C-c C-u") "undefined")))
  


(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(require 'rust-mode)
(require 'use-package)
(require 'agda2-mode)
(require 'racket-mode)

(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))


(add-hook 'agda2-mode-hook
  (lambda ()
    (define-key agda2-mode-map (kbd "C-i") nil)
    (define-key agda2-mode-map (kbd "RET") 'newline-and-indent)))

(add-hook 'python-mode-hook
  (lambda ()
    (define-key python-mode-map (kbd "RET") 'newline-and-indent)))


(eval-after-load 'agda2-mode
  '(progn
     (electric-indent-mode -1)
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
