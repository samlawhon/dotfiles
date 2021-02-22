;; =======================================================================
;; Julia
;; =======================================================================
(use-package julia-mode :ensure julia-mode)
(use-package julia-repl :ensure julia-repl)
(use-package flycheck-julia :ensure flycheck-julia)
(add-hook 'julia-mode-hook 'julia-repl-mode)
(use-package eglot-jl :ensure eglot-jl)
 
;; FUTURE - julia-snail looks really cool, but very alpha, and not Windows ready

