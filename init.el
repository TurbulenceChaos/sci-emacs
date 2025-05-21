;;; init.el --- Load the full configuration
;; Sci-Emacs
;; Author: Peng Peng
;; Email:  211110103110@stu.just.edu.cn
;; GitHub: https://github.com/TurbulenceChaos/Sci-Emacs
;; Environment: (emacs "30.1" in WSL2 Ubuntu 24.04)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-basic)
(require 'init-evil)
(require 'init-format)
(require 'init-completion)
(require 'init-magit)
(require 'init-undo)
(require 'init-eglot)
(require 'init-latex)
(require 'init-emacs-jupyter)
(require 'init-wolfram)
(require 'init-org)


(provide 'init)
;;; init.el ends here
