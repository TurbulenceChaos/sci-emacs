;;; Org-mode configuration

;; Org-mode
(setq org-startup-numerated t)

(add-to-list 'load-path "~/.emacs.d/lisp/org-config")

;; Displays org-mode inline images in a sliced manner
(require 'org-sliced-images-config)

;; Org-latex
(require 'org-latex)

;; Emacs-jupyter
(require 'emacs-jupyter-config)

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)
   (python . t)
   (jupyter . t)))


(provide 'org-config)
