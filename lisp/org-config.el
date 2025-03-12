;;; Org-mode configuration

;; Org-mode
(setq org-startup-numerated t)

(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "tikz" t))
(setf org-format-latex-header (concat "% xelatex\n" org-format-latex-header))
(setq org-format-latex-options (plist-put org-format-latex-options :scale 3))

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)
   (python . t)
   (jupyter . t)))

(setq org-babel-default-header-args:latex '((:results . "graphics file")
					    (:imagemagick . "t")
					    (:fit . "yes")
					    (:iminoptions . "-density 300 -units pixelsperinch")
					    (:imoutoptions . "-quality 100 -alpha remove")
					    (:noweb . "yes")
					    (:comments . "link")
					    (:eval . "never-export")))

(setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                     (:kernel . "python3")
                                                     (:session . "jupyter-python")
                                                     (:comments . "link")
                                                     (:eval . "never-export")))

;; Org babel Jupyter-Wolfram-Language
(require 'wolfram-config)
(setq org-babel-default-header-args:jupyter-Wolfram-Language '((:async . "yes")
							       (:kernel . "wolframlanguage14.1")
							       (:session . "jupyter-wolfram-language")
							       (:results . "value drawer")
							       (:comments . "link")
							       (:eval . "never-export")))

;; Export both PDF and PNG files when executing an org-tikz block.
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'org-tikz-export-pdf-and-png)

;; Displays org-mode inline images in a sliced manner
(add-to-list 'load-path "~/.emacs.d/lisp-site/org-sliced-images")
(require 'org-sliced-images)

(setq org-sliced-images-round-image-height t)
(org-sliced-images-mode 1)

(defalias 'org-remove-inline-images 'org-sliced-images-remove-inline-images)
(defalias 'org-toggle-inline-images 'org-sliced-images-toggle-inline-images)
(defalias 'org-display-inline-images 'org-sliced-images-display-inline-images)

(add-hook 'org-mode-hook 'org-sliced-images-display-inline-images)

(add-hook 'org-babel-after-execute-hook
	  '(lambda ()
	     (org-sliced-images-remove-inline-images) (org-sliced-images-display-inline-images)))


(provide 'org-config)
