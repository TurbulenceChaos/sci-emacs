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

(unless (package-installed-p 'org-modern)
  (package-install 'org-modern))

(setq org-modern-table nil
      org-modern-block-fringe nil)
(global-org-modern-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)
   (python . t)
   (jupyter . t)))

(setq org-src-block-faces '(("emacs-lisp" (:background "#FFFFE0" :extend t))
			    ("latex" (:background "#E5FFB8" :extend t))
			    ("jupyter-python" (:background "thistle1" :extend t))
			    ("jupyter-Wolfram-Language" (:background "LightCyan1" :extend t))))

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

(unless (package-installed-p 'org-sliced-images)
  (package-vc-install "https://github.com/TurbulenceChaos/org-sliced-images.git"))

(setq org-sliced-images-consume-dummies t
      org-sliced-images-round-image-height t)

(org-sliced-images-mode 1)

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
	      (revert-buffer nil t t)
              (org-remove-inline-images)
              (let ((inhibit-message t))
                (save-buffer)))))

(provide 'init-org)
;;; init-org.el ends here
