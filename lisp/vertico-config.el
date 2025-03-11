;;; Vertico configuration

;; Vertico
(add-to-list 'load-path "~/.emacs.d/lisp-site/compat")
(require 'compat)
(add-to-list 'load-path "~/.emacs.d/lisp-site/vertico")
(require 'vertico)
(vertico-mode)

;; Orderless
(add-to-list 'load-path "~/.emacs.d/lisp-site/orderless")
(require 'orderless)
(setq completion-styles '(substring orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(provide 'vertico-config)
