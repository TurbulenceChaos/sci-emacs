(add-to-list 'load-path "~/.emacs.d/lisp-site/xah-wolfram-mode")
(require 'xah-wolfram-mode)

(defalias 'wolfram-language-mode 'xah-wolfram-mode)
(defalias 'wolfram-mode 'wolfram-language-mode)

(defun org-babel-execute:wolfram (body params)
  "Execute the given Wolfram block via `jupyter-Wolfram-Language`."
  (org-babel-execute:jupyter-Wolfram-Language body params))

(provide 'wolfram-config)
