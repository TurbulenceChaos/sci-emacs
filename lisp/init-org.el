;;; init-org.el --- Comprehensive Org-mode Configuration
;;; Commentary:
;;; This configuration file sets up advanced Org-mode functionality,
;;; including LaTeX preview, Babel language support, and image handling.

;;; Code:

(unless (package-installed-p 'org)
  (package-install 'org))
;; (require 'org)

;; org-basic
(setq org-startup-numerated t
      org-support-shift-select t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0
      org-babel-min-lines-for-block-output 1000
      org-startup-with-latex-preview t
      org-preview-latex-image-directory "tmp/ltximg/")
(require 'ox-latex)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 3))
(with-eval-after-load 'org-faces
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch :height 0.85))
(with-eval-after-load 'org
  (custom-set-faces
   `(line-number-current-line ((t (:inherit line-number
					    :foreground ,(face-attribute 'org-done :foreground nil t)
					    :background ,(face-attribute 'org-done :background nil t)
					    :box nil
					    :weight bold))))))

;; org-theme
(unless (package-installed-p 'org-modern)
  (package-install 'org-modern))
;; (require 'org-modern)

(setq org-modern-table nil)
(setq org-modern-block-fringe nil)
(global-org-modern-mode)

;; org-sliced-images
(unless (package-installed-p 'org-sliced-images)
  (package-vc-install "https://github.com/TurbulenceChaos/org-sliced-images.git"))
;; (require 'org-sliced-images)

(setq org-sliced-images-round-image-height t)

(defun my/org-sliced-images-display-inline-images ()
  "Remove org-sliced-images before displaying it."
  (interactive)
  (org-sliced-images-remove-inline-images)
  (org-sliced-images-display-inline-images))

(defalias 'org-remove-inline-images 'org-sliced-images-remove-inline-images)
(defalias 'org-toggle-inline-images 'org-sliced-images-toggle-inline-images)
(defalias 'org-display-inline-images 'my/org-sliced-images-display-inline-images)

(defun quiet-save-buffer ()
  "Save current buffer without message."
  (interactive)
  (let ((inhibit-message t))
    (save-buffer)))

(add-hook 'org-mode-hook
          (lambda ()
            (org-sliced-images-display-inline-images)
            (quiet-save-buffer)))

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
	      (org-sliced-images-remove-inline-images)
	      (quiet-save-buffer))))

;; org-babel
(setq org-src-block-faces '(("emacs-lisp" (:background "#E5FFB8" :extend t)) ;; #EEE2FF, #E5FFB8, gray90
			    ("jupyter-python" (:background "thistle1" :extend t))
			    ("jupyter-Wolfram-Language" (:background "LightCyan1" :extend t))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)
   (python . t)
   (jupyter . t)))

;; latex-tikz
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
        (:eval . "never-export")))

;; jupyter-python
(setq org-babel-default-header-args:jupyter-python
      '((:async . "yes")
        (:kernel . "python3")
        (:session . "jupyter-python")
        (:results . "value drawer")
        (:comments . "link")
        (:eval . "never-export")))

;; jupyter-Wolfram-Language
(with-eval-after-load 'org
  (defalias 'wolfram-language-mode 'xah-wolfram-mode))

(setq org-babel-default-header-args:jupyter-Wolfram-Language
      '((:async . "yes")
        (:kernel . "wolframlanguage14.1")
        (:session . "jupyter-wolfram-language")
        (:results . "value drawer")
        (:display . "text")
        (:comments . "link")
        (:eval . "never-export")))

;; wolfram-terminal-image for processing jupyter-wolfram-language results
(unless (package-installed-p 'wolfram-terminal-image)
  (package-vc-install
   '(wolfram-terminal-image :url "https://github.com/TurbulenceChaos/Wolfram-terminal-image.git"
			    :branch "main")))
;; (require 'wolfram-terminal-image)

;; t (default) for converting wolfram formula to latex;
;; otherwise nil for converting wolfram formula to image
(setq wolfram-terminal-formula-type=latex t)

;; minimum number of lines for block output
(setq org-babel-min-lines-for-block-output 1000)


(provide 'init-org)
;;; init-org.el ends here
