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
(global-set-key (kbd "C-k") 'magit)

;; evil - vim keybindings
(use-package evil :ensure evil)
(global-set-key (kbd "C-;") 'evil-mode)

;; Color theme
(use-package jetbrains-darcula-theme
  :ensure jetbrains-darcula-theme
  :config
  (load-theme 'jetbrains-darcula t))

;; =======================================================================
;; Projectile - Project Management
;; =======================================================================
(use-package helm :ensure t)
(use-package projectile :ensure t)
(projectile-mode)
(setq projectile-completion-system 'helm)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-S-n") 'projectile-find-file)


;; =======================================================================
;; C/C++
;; =======================================================================
(global-set-key (kbd "<f5>") 'compile)


;; =======================================================================
;; Misc. Key command stuff
;; =======================================================================

;; =======================================================================
;; Language config sources
;; =======================================================================
(load-file (concat user-emacs-directory "scala.el"))
(load-file (concat user-emacs-directory "python.el"))
(load-file (concat user-emacs-directory "julia.el"))


;; =======================================================================
;; Wishlist
;; =======================================================================
;; TODO highlight matching paren
;; TODO search for symbol on C-M-S-n
;; TODO search for class on C-n
;; TODO hook for electric-pair mode
;; TODO C-w and C-S-w for syntax-aware expansion/contraction
;; TODO don't have vim-mode shadow useful key idea-like commands
;; TODO add hooks for each language in their source files
;; TODO C-S-j for smart join line
;; TODO highlight matching instances of word under cursor

(provide 'init)
;;; init.el ends here
