;;; init-org.el --- Org-mode config
;;; Commentary:
;;; Code:

(setq org-startup-numerated t
      org-support-shift-select t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0
      org-startup-with-latex-preview t
      org-startup-with-inline-images t
      org-preview-latex-image-directory "tmp/ltximg/")

(with-eval-after-load 'org
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)
   (jupyter . t)))

(add-to-list 'org-latex-packages-alist '("" "tikz" t))
(setq org-format-latex-header (concat "% xelatex\n" org-format-latex-header))
(setq org-babel-default-header-args:latex 
      '((:results . "graphics file")
        (:imagemagick . "t")
        (:fit . "yes")
        (:iminoptions . "-density 300 -units pixelsperinch")
        (:imoutoptions . "-quality 100 -alpha remove")
        (:noweb . "yes")
        (:comments . "link")
        (:eval . "never-export")))

(setq org-babel-default-header-args:jupyter-python
      '((:async . "yes")
        (:kernel . "python3")
        (:session . "jupyter-python")
        (:results . "value drawer")
        (:comments . "link")
        (:eval . "never-export")))

(defalias 'wolfram-language-mode 'xah-wolfram-mode)
(setq org-babel-default-header-args:jupyter-Wolfram-Language
      '((:async . "yes")
        (:kernel . "wolframlanguage14.1")
        (:session . "jupyter-wolfram-language")
        (:results . "value drawer")
        (:display . "text")
        (:comments . "link")
        (:eval . "never-export")))

(unless (package-installed-p 'Wolfram-terminal-image)
  (package-vc-install "https://github.com/TurbulenceChaos/Wolfram-terminal-image.git"))

(if (setq wolfram-terminal-formula-type=latex nil)
    (setq org-babel-min-lines-for-block-output 100)
  (setq org-babel-min-lines-for-block-output 20))


(provide 'init-org)
;;; init-org.el ends here
