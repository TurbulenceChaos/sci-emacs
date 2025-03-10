;; Org-config
(add-to-list 'org-latex-packages-alist '("" "tikz" t))
(setf org-format-latex-header (concat "% xelatex\n" org-format-latex-header))
(setq org-format-latex-options (plist-put org-format-latex-options :scale 3))
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
					    (:iminoptions . "-density 300 -units pixelsperinch")
					    (:imoutoptions . "-quality 100 -alpha remove")
					    (:noweb . "yes")
					    (:eval . "never-export")))

(provide 'org-config)
