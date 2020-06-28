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

;; TODO - use this?
;; leaf is for more advanced control compared to use-package
;; (use-package leaf :ensure leaf)


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


;; =======================================================================
;; Things set by CUSTOM
;; =======================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages (quote (evil magit markdown-mode simpleclip julia-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2d3743" :foreground "#e1e1e0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "outline" :family "DejaVu Sans Mono for Powerline")))))



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

;; evil - vim keybindings
(use-package evil :ensure evil)

;; TODO - key-chord

;; =======================================================================
;; Julia
;; =======================================================================
(use-package julia-mode :ensure julia-mode)
 
;; FUTURE - julia-snail looks really cool, but very alpha, and not Windows ready
;; FUTURE - julia-repl for interactive, NOT julia-shell


;; TODO Lookup:
;; add-to-list with t at the end
