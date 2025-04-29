;;; init.el --- Load the full configuration
;; Sci-Emacs
;; Author: Peng Peng
;; Email:  211110103110@stu.just.edu.cn
;; GitHub: https://github.com/TurbulenceChaos/Sci-Emacs
;; Environment: (emacs "30.1" in WSL2 Ubuntu 24.04)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-basic)
(require 'init-theme)
(require 'init-format)
(require 'init-evil)
(require 'init-completion)
(require 'init-snippet)
(require 'init-magit)
(require 'init-undo)
(require 'init-modeline)
(require 'init-keycast)
(require 'init-treemacs)
(require 'init-dashboard)
(require 'init-window)
(require 'init-emacs-jupyter)
(require 'init-wolfram)
(require 'init-latex)
(require 'init-org)


(provide 'init)
;;; init.el ends here
