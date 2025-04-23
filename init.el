;;; init.el --- Load the full configuration
;; SCI-emacs
;; Author: Peng Peng
;; Email:  211110103110@stu.just.edu.cn
;; GitHub: https://github.com/TurbulenceChaos/SCI-emacs
;; Environment: (emacs "30.1" in WSL2 Ubuntu 24.04)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-basic)
(require 'init-completion)
(require 'init-magit)
(require 'init-undo)
(require 'init-keycast)
(require 'init-latex)
(require 'init-wolfram)
(require 'init-emacs-jupyter)
(require 'init-org)


(provide 'init)
;;; init.el ends here
