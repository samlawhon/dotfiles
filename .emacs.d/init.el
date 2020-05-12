;; ============================================================================
;; package initialization
;; ============================================================================

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(color-theme-sanityinc-tomorrow
    magit
    flx-ido
    projectile
    linum-relative
    jdee
    julia-mode
    julia-repl
    elpy
    pyvenv
    arc-dark-theme
    markdown-mode
    lsp-mode
    lsp-ui
    yasnippet
    company-lsp
    posframe
    dap-mode
    lsp-treemacs
    lsp-java
    flycheck
    py-autopep8
    sbt-mode
    scala-mode
    use-package))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#424242" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#eaeaea"))
 '(beacon-color "#d54e53")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(ein:output-area-inlined-images t)
 '(fci-rule-color "#424242")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (lsp-mode ## groovy-mode gradle-mode memoize jdee magit color-theme-sanityinc-tomorrow solarized-theme)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; ============================================================================
;; Visuals
;; ============================================================================

(setq inhibit-splash-screen t)
(global-display-line-numbers-mode)
(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow-night)
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(setq column-number-mode t)
(display-time-mode)
(require 'linum-relative)
(linum-relative-global-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(require 'markdown-mode)
(require 'julia-mode)
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)
(require 'yasnippet)
(yas-global-mode 1)


;; ============================================================================
;; Preferences
;; ============================================================================

;; Stop stupid bell
(setq ring-bell-function 'ignore)

;; Don't clutter up directories with files~
(setq backup-directory-alist
      `(("." . ,"~/.emacs.d/backups")))

;; Don't clutter with #files either
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/backups")))

;; Enable copy to clipboard
(setq select-enable-clipboard t)

;; Enable split-window dired copying
(setq dired-dwim-target t)

;; Enable use-package
(require 'use-package)




;; ============================================================================
;; Python
;; ============================================================================

;; elpy setup
(elpy-enable)
(setq elpy-rpc-virtualenv-path 'current)
(setenv "WORKON_HOME" "~/miniconda3/envs")  ; TODO work on an auto-lookup
(pyvenv-mode 1)
(pyvenv-workon 'gsk)

;; Use IPython for REPL
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;; 	     "jupyter")

;; Enable flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode_hook 'flycheck-mode))

;; Jupyter
(package-initialize)
(require 'ein)
(require 'ein-notebook)

;; Debugging
;; learn how to make this a function
;; (global-set-key [f9] (pdb "python3 -m pdb"))
;; (global-set-key [f8] )


;; ============================================================================
;; Scala (mostly copied from https://scalameta.org/metals/docs/editors/emacs.html)
;; ============================================================================

;; Enable scala-mode for highlighting, indentation, and motion controls
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

;; Enable sbt-mode for exwcuting sbt commands
(use-package sbt-mode
   :commands sbt-start sbt-command
   :config
   ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
   ;; allows using SPACE when in the minibuffer
   (substitute-key-definition
    'minibuffer-complete-word
    'self-insert-command
    minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )

;; Enable nice rendering of diagnostics like compile errors
(use-package flycheck :init (global-flycheck-mode))

(use-package lsp-mode
	     ;; enable lsp-mode automatically in scala files
	     :hook (scala-mode . lsp)
	           (lsp-mode . lsp-lens-mode)
		   :config (setq lsp-diagnostic-package nil))

;; Enable nice rencering of documentation on hover
(use-package lsp-ui)

;; Use yasnippets
(use-package yasnippet)

;; Add company-lsp backend for metals
(use-package company-lsp)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe)

(use-package dap-mode
	     :hook
	     (lsp-mode . dap-mode)
	     (lsp-mode . dap-ui-mode))

;; Use the Tree View Protocol for viewing the project structure and triggering compilation
(use-package lsp-treemacs :config (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t))


;; ============================================================================
;; Julia
;; ============================================================================

(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode)  

;; ============================================================================
;; Projectile
;; ============================================================================

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-globally-ignored-directories `(".bloop", ".metals"))


;; ============================================================================
;; Magit
;; ============================================================================

(global-set-key [f5] (quote magit-status))


;; ============================================================================
;; Key-chord
;; ============================================================================
(add-to-list 'load-path "~/.emacs.d/key-chord")
(require 'key-chord)
(setq key-chord-two-keys-delay 0.05)
(key-chord-mode 1)


;; ============================================================================
;; Evil
;; ============================================================================

;; Load evil
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)

;; Keybindings
(key-chord-define-global "jk" 'evil-mode)
(key-chord-define-global "kj" 'evil-mode)


;; ============================================================================
;; Start server for faster subsequent startups
;; ============================================================================
(server-start)
