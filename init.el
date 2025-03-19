;;; SCI emacs
;; Author: Peng Peng
;; Email: 211110103110@stu.just.edu.cn
;; Homepage: https://github.com/TurbulenceChaos/SCI-emacs
;; Package-Requires: (emacs "30.1")

;; Org-mode
(add-to-list 'load-path "~/.emacs.d/lisp/org-config")
(require 'org-config)

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Comment divider
(require 'comment-divider)

;; Completion
(require 'completion-config)

;; Magit
(require 'magit-config)

;; Undo history
(require 'undo-config)

