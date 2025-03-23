;;; Org-latex configuration

(require 'ox-latex)
(setq org-startup-with-latex-preview t)
(setq org-preview-latex-image-directory "tmp/ltximg/")

(add-to-list 'org-latex-packages-alist '("" "tikz" t))
(setf org-format-latex-header (concat "% xelatex\n" org-format-latex-header))
(setq org-format-latex-options (plist-put org-format-latex-options :scale 3))

;; Export both PDF and PNG files when executing an org-tikz block.
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'org-tikz-export-pdf-and-png)

(setq org-babel-default-header-args:latex '((:results . "graphics file")
					    ;; (:results . "value drawer")				
					    (:imagemagick . "t")
					    (:fit . "yes")
					    (:iminoptions . "-density 300 -units pixelsperinch")
					    (:imoutoptions . "-quality 100 -alpha remove")
					    (:noweb . "yes")
					    (:comments . "link")
					    (:eval . "never-export")))

(provide 'org-latex)
