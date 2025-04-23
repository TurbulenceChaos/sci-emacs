;;; init.el --- Load the full configuration
;; SCI-emacs
;; Author: Peng Peng
;; Email:  211110103110@stu.just.edu.cn
;; GitHub: https://github.com/TurbulenceChaos/SCI-emacs
;; Package-Requires: (emacs "30.1")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-basic)
(require 'init-completion)
(require 'init-magit)
(require 'init-undo)
(require 'init-keycast)
(require 'init-emacs-jupyter)
(require 'init-wolfram)
(require 'init-org)


(provide 'init)
;;; init.el ends here
