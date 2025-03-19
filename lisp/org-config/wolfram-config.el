(add-to-list 'load-path "~/.emacs.d/lisp-site/xah-wolfram-mode")
(require 'xah-wolfram-mode)

(defalias 'wolfram-language-mode 'xah-wolfram-mode)
(defalias 'wolfram-mode 'wolfram-language-mode)

(defun org-babel-execute:wolfram (body params)
  "Execute the given Wolfram block via `jupyter-Wolfram-Language`."
  (org-babel-execute:jupyter-Wolfram-Language body params))

(setq my/org-babel-wolfram-headers
      '((:async . "yes")
        (:kernel . "wolframlanguage14.1")
        (:session . "jupyter-wolfram-language")
        (:results . "value drawer")
        (:comments . "link")
        (:eval . "never-export")))

(setq org-babel-default-header-args:jupyter-Wolfram-Language my/org-babel-wolfram-headers)
(setq org-babel-default-header-args:wolfram my/org-babel-wolfram-headers)

(provide 'wolfram-config)
