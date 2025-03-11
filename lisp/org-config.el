;;; Org-mode configuration

;; Org-mode
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "tikz" t))
(setf org-format-latex-header (concat "% xelatex\n" org-format-latex-header))
(setq org-format-latex-options (plist-put org-format-latex-options :scale 3))

(setq org-image-actual-width '(0.5))
(setq org-startup-with-inline-images t)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("emacs-lisp" "latex"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-babel-default-header-args:latex '((:results . "graphics file")
					    (:imagemagick . "t")
					    (:fit . "yes")
					    (:iminoptions . "-density 600 -units pixelsperinch")
					    (:imoutoptions . "-quality 100 -alpha remove")
					    (:noweb . "yes")
					    (:eval . "never-export")))

;; Displays org-mode inline images in a sliced manner
(add-to-list 'load-path "~/.emacs.d/lisp-site/org-sliced-images")
(require 'org-sliced-images)

(setq org-sliced-images-round-image-height t)
(org-sliced-images-mode 1)

;; Export both PDF and PNG files when executing an org-tikz block.
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'org-tikz-export-pdf-and-png)

(provide 'org-config)
