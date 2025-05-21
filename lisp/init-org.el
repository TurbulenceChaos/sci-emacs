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
        (:results . "value drawer")
        (:imagemagick . "t")
        (:fit . "yes")
        (:iminoptions . "-density 300 -units pixelsperinch")
        (:imoutoptions . "-quality 100 -alpha remove")
        (:noweb . "yes")
        (:comments . "link")
        (:eval . "never-export")
        (:exports . "both")))

(setq org-babel-default-header-args:jupyter-python
      '((:async . "yes")
        (:kernel . "python3")
        (:session . "jupyter-python")
        (:results . "value drawer")
        (:comments . "link")
        (:eval . "never-export")
        (:exports . "both")))

(setq org-babel-default-header-args:jupyter-Wolfram-Language
      '((:async . "yes")
        (:kernel . "wolframlanguage14.1")
        (:session . "jupyter-wolfram-language")
        (:results . "value drawer")
        (:display . "text")
        (:comments . "link")
        (:eval . "never-export")
        (:exports . "both")))

(unless (package-installed-p 'org-sliced-images)
  (package-vc-install "https://github.com/TurbulenceChaos/org-sliced-images.git"))

(setq org-sliced-images-consume-dummies t
      org-sliced-images-round-image-height t)

(org-sliced-images-mode 1)

(defun remove-sliced-images ()
  (when (buffer-modified-p)
    (revert-buffer nil t t))
  (org-remove-inline-images))

(add-hook 'kill-buffer-hook
	  (lambda ()
	    (when (eq major-mode 'org-mode)
	      (if (bound-and-true-p org-export-current-backend)
		  (lambda (backend) (remove-sliced-images))
		(progn
		  (remove-sliced-images)
		  (let ((inhibit-message t))
                    (save-buffer)))))))


(provide 'init-org)
;;; init-org.el ends here
