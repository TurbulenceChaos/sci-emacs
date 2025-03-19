;;; Emacs-jupyter configuration

(add-to-list 'load-path "~/.emacs.d/lisp-site/markdown-mode")
(require 'markdown-mode)

(add-to-list 'load-path "~/.emacs.d/lisp-site/jupyter/.eldev/30.1/packages/websocket-1.15")
(require 'websocket)

(add-to-list 'load-path "~/.emacs.d/lisp-site/jupyter/.eldev/30.1/packages/simple-httpd-1.5.1")
(require 'simple-httpd)

(add-to-list 'load-path "~/.emacs.d/lisp-site/jupyter/.eldev/30.1/packages/zmq-1.0.2")
(require 'zmq)

(add-to-list 'load-path "~/.emacs.d/lisp-site/jupyter")
(require 'jupyter)

(setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                     (:kernel . "python3")
                                                     (:session . "jupyter-python")
                                                     (:comments . "link")
                                                     (:eval . "never-export")))

;; Org babel Jupyter-Wolfram-Language
(require 'wolfram-config)

(provide 'emacs-jupyter-config)
