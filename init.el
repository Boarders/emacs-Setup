(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups
;; save on loss of focus
(defun save-all ()
    (interactive)
    (save-some-buffers t))
  (add-hook 'focus-out-hook 'save-all)
(load-theme 'tango-dark t) ; load custom theme here
(setq column-number-mode t)
(add-hook 'prog-mode-hook 'linum-mode) ; turn on line number mode in all programming environments
(global-whitespace-mode 1)
(setq whitespace-line-column 140)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
;; add key to run makefile in project
(global-set-key (kbd "C-c m") 'recompile)
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
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-new-repl))
 '(package-selected-packages
   (quote
    (multiple-cursors merlin tuareg use-package dante rust-mode intero idris-mode haskell-mode))))
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
 '(agda2-highlight-string-face ((t (:foreground "Orange")))))
(load-file (let ((coding-system-for-read 'utf-8))
         (shell-command-to-string "agda-mode locate")))

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "C-c C-u") "undefined")
  (define-key haskell-mode-map (kbd "C-c 8") "\
----------------------------------------------------------------\
-- |                                                         |--\
----------------------------------------------------------------")))

(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(add-to-list 'load-path "~/.emacs.d/libs/purescript-mode/")
(require 'purescript-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.emacs.d/libs/purescript-mode/")

(require 'rust-mode)
(require 'flycheck)
(require 'use-package)

(add-hook 'purescript-mode-hook
  (lambda ()
    (flycheck-mode)
    (turn-on-purescript-indentation)))

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

(eval-after-load 'agda-mode
  '(progn
     (electric-indent-mode -1)
   )
)

;; load ghcid
(add-to-list 'load-path "~/.emacs.d/libs/ghcid")
(load "ghcid")

;; ocaml set-up
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

(require 'ocp-indent)
(require 'merlin)
(require 'tuareg)

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  (autoload 'merlin-mode "merlin" nil t nil)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)))

;; multiple cursors set up
(require 'multiple-cursors)
(global-set-key (kbd "C-i") nil)
(global-set-key (kbd "C-i C-i") 'mc/edit-lines)
