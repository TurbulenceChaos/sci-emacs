;;; init.el --- Load the full configuration
;; Sci-Emacs
;; Author: Peng Peng
;; Email:  211110103110@stu.just.edu.cn
;; GitHub: https://github.com/TurbulenceChaos/Sci-Emacs
;; Environment: (emacs "30.1" in WSL2 Ubuntu 24.04)

;; basic
(set-background-color "honeydew")
(set-face-attribute 'default nil :height 240)
(global-prettify-symbols-mode 1)
(electric-pair-mode t)
(setq hscroll-step 1
      scroll-conservatively 10
      ring-bell-function 'ignore)

;; vim
(unless (package-installed-p 'evil)
  (package-install 'evil))

(with-eval-after-load 'evil
  (evil-set-undo-system 'undo-redo))

(evil-mode)

;; completion
(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(which-key-mode)

(setq icomplete-compute-delay 0
      icomplete-delay-completions-threshold 0
      icomplete-max-delay-chars 0
      icomplete-scroll t
      icomplete-show-matches-on-no-input t)
(icomplete-vertical-mode)
(define-key icomplete-vertical-mode-minibuffer-map (kbd "TAB") 'icomplete-force-complete)
(define-key icomplete-vertical-mode-minibuffer-map (kbd "RET") 'icomplete-force-complete-and-exit)
(define-key icomplete-vertical-mode-minibuffer-map (kbd "SPC") 'self-insert-command)
(define-key icomplete-vertical-mode-minibuffer-map (kbd "C-j") 'exit-minibuffer)

(unless (package-installed-p 'orderless)
  (package-install 'orderless))

(setq completion-styles '(basic orderless substring )
      completion-category-overrides '((file (basic styles partial-completion))))

(unless (package-installed-p 'magit)
  (package-install 'magit))

;; markdown
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;; latex
(unless (package-installed-p 'auctex)
  (package-install 'auctex))

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; jupyter
(unless (package-installed-p 'jupyter)
  (package-install 'jupyter))

;; wolfram
(unless (package-installed-p 'sci-wolfram)
  (package-vc-install "https://github.com/TurbulenceChaos/sci-wolfram.git"))

;; org
(setq org-startup-numerated t
      org-confirm-babel-evaluate nil
      org-babel-min-lines-for-block-output 100
      org-edit-src-content-indentation 0
      org-startup-with-latex-preview t
      org-startup-with-inline-images t)

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

(with-eval-after-load 'org
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5)))
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

(unless (package-installed-p 'org-sliced-images)
  (package-vc-install "https://github.com/TurbulenceChaos/org-sliced-images.git"))


(provide 'init)
;;; init.el ends here
