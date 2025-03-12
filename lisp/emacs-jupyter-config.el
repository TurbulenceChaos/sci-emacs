;;; Emacs-jupyter configuration

(add-to-list 'load-path "~/.emacs.d/lisp-site/markdown-mode")
(require 'markdown-mode)

(add-to-list 'load-path "~/.emacs.d/lisp-site/emacs-websocket")
(require 'websocket)

(add-to-list 'load-path "~/.emacs.d/lisp-site/emacs-web-server")
(require 'simple-httpd)

(add-to-list 'load-path "~/.emacs.d/lisp-site/emacs-zmq")
(require 'zmq)

(add-to-list 'load-path "~/.emacs.d/lisp-site/jupyter")
(require 'jupyter)

(add-to-list 'load-path "~/.emacs.d/lisp-site/xah-wolfram-mode")
(require 'xah-wolfram-mode)
(defalias 'wolfram-language-mode 'xah-wolfram-mode)

(provide 'emacs-jupyter-config)
