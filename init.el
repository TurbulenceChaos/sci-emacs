;;; SCI emacs - Peng 2025
;;; GitHub: https://github.com/TurbulenceChaos/SCI-emacs

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Comment divider
(require 'comment-divider)

;; Org-mode
(require 'org-config)

;; Vertico
(require 'vertico-config)

;; Magit
(require 'magit-config)

;; Undo history
(require 'undo-config)
