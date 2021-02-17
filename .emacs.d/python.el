;;; package --- summary
;; Python mode configuration
;;; Commentary:
;;; Code
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

(add-hook 'python-mode-hook (electric-pair-mode t))
(provide 'python)
;;; python.el ends here
