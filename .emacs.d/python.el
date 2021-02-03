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

