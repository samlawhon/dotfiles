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

;; evil - vim keybindings
(use-package evil :ensure evil)
(global-set-key (kbd "C-;") 'evil-mode)


;; =======================================================================
;; Projectile - Project Management
;; =======================================================================
(use-package helm :ensure t)
(use-package projectile :ensure t)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; =======================================================================
;; Julia
;; =======================================================================
(use-package julia-mode :ensure julia-mode)
(use-package julia-repl :ensure julia-repl)
(use-package flycheck-julia :ensure flycheck-julia)
(add-hook 'julia-mode-hook 'julia-repl-mode)
(use-package eglot-jl :ensure eglot-jl)
 
;; FUTURE - julia-snail looks really cool, but very alpha, and not Windows ready


;; =======================================================================
;; Python
;; =======================================================================
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython")
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt")
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1))))

(use-package sphinx-doc
  :ensure t
  :hook (python-mode . sphinx-doc-mode))


;; =======================================================================
;; C/C++
;; =======================================================================
(global-set-key (kbd "<f5>") 'compile)

;; =======================================================================
;; Things set by "Custom"
;; =======================================================================
(use-package python-docstring
  :ensure t
  :config (setq python-docstring-sentence-end-double-space nil)
  :hook (python-mode . python-docstring-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes '(solarized-wombat-dark))
 '(custom-safe-themes
   '("13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" default))
 '(package-selected-packages
   '(lsp-metals sbt-mode ammonite-term-repl eglot-jl ein vterm projectile helm use-package sphinx-doc simpleclip python-docstring noflet markdown-mode magit kv key-chord julia-repl julia-mode iceberg-theme flycheck-julia evil elpy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; =======================================================================
;; Metals
;; =======================================================================
(require 'package)

;; Add melpa to your packages repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
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

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
  :config (setq lsp-prefer-flymake nil))

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :config (setq lsp-metals-treeview-show-when-views-received t))

;; Enable nice rendering of documentation on hover
(use-package lsp-ui)

;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;;   to avoid odd behavior with snippets and indentation
(use-package yasnippet)

;; Add company-lsp backend for metals
(use-package company-lsp)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )


;; =======================================================================
;; Misc. Key command stuff
;; =======================================================================
(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: '\"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
This command select between any bracket chars, not the inner text of a bracket. For example, if text is

 (a(b)c▮)

 the selected char is “c”, not “a(b)c”.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2020-03-11"
  (interactive)
  (let (
        ($skipChars "^'\"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）〘〙")
        $p1
        )
    (skip-chars-backward $skipChars)
    (setq $p1 (point))
    (skip-chars-forward $skipChars)
    (set-mark $p1)))

(global-set-key (kbd "C-.") 'xah-select-text-in-quote)
