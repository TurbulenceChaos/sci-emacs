;;; SCI emacs
;; Author: Turbulence <211110103110@stu.just.edu.cn>
;; Homepage: https://github.com/TurbulenceChaos/SCI-emacs
;; Package-Requires: (emacs "30.1")

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Comment divider
(require 'comment-divider)

;; Emacs-jupyter
(require 'emacs-jupyter-config)

;; Org-mode
(require 'org-config)

;; Vertico
(require 'vertico-config)

;; Magit
(require 'magit-config)

;; Undo history
(require 'undo-config)
