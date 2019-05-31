;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq visible-bell 1)

;; UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
(global-display-line-numbers-mode)


;; Font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-9"))


;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)


;; Colorschemes
(add-to-list 'load-path "~/emacs-plugins/emacs-doom-themes")
(require 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-one t)
