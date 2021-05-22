;;; init.el --- my emacs init
;;
;; Author : Robert Enzmann
;;
;;; Commentary:
;; I put this here to make the linter stop whining.
;;
;;; Code
;; =======================================================================
;; Package management
;; =======================================================================
;; Enable MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "Your version of Emacs does not support SSL connections."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  )
(package-initialize)


;; Install use-package if it hasn't been already
(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package))
)

(require 'use-package)


;; =======================================================================
;; Basic preferences
;; =======================================================================

;; Inhibit splash screen
(setq inhibit-splash-screen t)

;; Stop stupid bell
(setq ring-bell-function 'ignore)

;; Enable split-window dired copying
(setq dired-dwim-target t)

;; No need for the toolbar in GUI mode
(tool-bar-mode -1)
(menu-bar-mode -1)

;; If in a GUI, set the window a bit bigger and more centered
(if (window-system) (set-frame-size (selected-frame) 124 50))
(if (window-system) (set-frame-position (selected-frame) 400 60))

;; Line and number modes
(global-linum-mode)
(column-number-mode)

;; Automatically create matching parens in lisp mode
(add-hook 'lisp-mode (electric-pair-mode t))

;; Make IDO the default
(require 'ido)
(ido-mode t)
;(setq ido-enable-flex-matching t)
;(setq ido-everywhere t)
;(ido-enable 1)

;; Follow symlinks to the real file
(setq vc-follow-symlinks t)

;; =======================================================================
;; Plugins
;; =======================================================================

;; Provides clipboarding to the outside OS
;; Copy:  C-<Ins>
;; Paste: S-<Ins>
;; Cut:   S-<Del>
(use-package simpleclip :ensure simpleclip)

;; Support for markdown, require when needed
(use-package markdown-mode :ensure markdown-mode)

;; Version control (magit)
(use-package magit :ensure magit)
(global-set-key (kbd "C-x C-k") 'magit)

;; evil - vim keybindings
(use-package evil :ensure evil)
(global-set-key (kbd "C-;") 'evil-mode)

;; Color theme
(use-package jetbrains-darcula-theme
  :ensure jetbrains-darcula-theme
  :config
  (load-theme 'jetbrains-darcula t))

(use-package yaml-mode :ensure yaml-mode)

;; =======================================================================
;; Projectile - Project Management
;; =======================================================================
(use-package helm :ensure t)
(use-package projectile :ensure t)
(projectile-mode)
(setq projectile-completion-system 'helm)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-S-n") 'projectile-find-file)
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'native)

;; =======================================================================
;; C/C++
;; =======================================================================
(global-set-key (kbd "<f5>") 'compile)


;; =======================================================================
;; Misc. Key command stuff
;; =======================================================================
(defun find-user-init-file ()
  "Opens your init file."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-M-s") 'find-user-init-file)

(global-set-key (kbd "C-S-<del>") 'goto-last-change)

;; =======================================================================
;; Language config sources
;; =======================================================================
(load-file (concat user-emacs-directory "scala.el"))
(load-file (concat user-emacs-directory "python.el"))
(load-file (concat user-emacs-directory "julia.el"))


;; =======================================================================
;; Wishlist
;; =======================================================================
;; (python) syntax highlighting within {} when inside f-string
;; modify movement to match normal editors, e.g. ctl-arrow to parens
;; highlight matching paren
;; search for symbol on C-M-S-n
;; search for class on C-n
;; hook for electric-pair mode
;; C-w and C-S-w for syntax-aware expansion/contraction
;; don't have vim-mode shadow useful key idea-like commands
;; add hooks for each language in their source files
;; C-S-j for smart join line
;; highlight matching instances of word under cursor
;; C-S-<BS> for jump to last edit
;; M-S-<F9> debug menu
;; M-S-<F10> run menu
;; C-q documentation
;; C-p parameter info
;; C-S-; fullsize/reset window
;; Install metals LSP

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode jetbrains-darcula-theme use-package sphinx-doc simpleclip scala-mode sbt-mode python-docstring projectile magit lsp-ui lsp-metals julia-repl helm flycheck-julia evil elpy eglot-jl company-lsp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
