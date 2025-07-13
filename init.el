;;; init.el --- Load the full configuration
;; Sci-Emacs
;; Author: Peng Peng
;; Email: 211110103110@stu.just.edu.cn
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

;; eww
(add-hook 'eww-after-render-hook 'eww-readable)

;; vim
(unless (package-installed-p 'evil)
  (package-install 'evil))

(with-eval-after-load 'evil
  (evil-set-undo-system 'undo-redo))

(evil-mode)

;; undo
(unless (package-installed-p 'undo-fu-session)
  (package-install 'undo-fu-session))

(undo-fu-session-global-mode)

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

(setq completion-styles '(basic orderless substring)
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

;; tools
(setq sci-emacs-tools-leader-key "<f5>")

(defun sci-emacs-insert-date ()
  "Insert date in current buffer, equal to `C-u M-! date RET'"
  (interactive)
  (shell-command "date" t))

(defun sci-emacs-open-init-file ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun sci-emacs-oxford-dic-lookup ()
  (interactive)
  (let* ((word (read-string "Enter word that you want to search in oxford dictionary:"))
         (url (format "https://www.oxfordlearnersdictionaries.com/definition/english/%s" word)))
    (browse-url url)))

(defun sci-emacs-wiki-lookup ()
  (interactive)
  (let* ((word (upcase-initials (read-string "Enter word that you want to search in wiki:")))
	 (url (format "https://en.wikipedia.org/wiki/%s" word)))
    (eww url)))

(defun sci-emacs-gramma-check ()
  (interactive)
  (let* ((sentence
	  (if (region-active-p)
	      (buffer-substring-no-properties (region-beginning) (region-end))
	    (buffer-substring-no-properties
	     (save-excursion (backward-paragraph) (point))
	     (save-excursion (forward-paragraph) (point)))))
         (url "https://engnovate.com/is-this-grammatically-correct/"))
    (kill-new sentence)
    (browse-url url)))

(defun sci-emacs-wsl-open-external ()
  "Open a file or directory in Windows."
  (interactive)
  (let* ((file (read-file-name "Select file or directory to open: "))
         (wsl-path (shell-quote-argument (expand-file-name file))))
    (shell-command (format "explorer.exe $(wslpath -w %s)" wsl-path))))

(global-set-key (kbd (concat sci-emacs-tools-leader-key " i")) #'sci-emacs-open-init-file)
(global-set-key (kbd (concat sci-emacs-tools-leader-key " h")) #'sci-emacs-oxford-dic-lookup)
(global-set-key (kbd (concat sci-emacs-tools-leader-key " w")) #'sci-emacs-wiki-lookup)
(global-set-key (kbd (concat sci-emacs-tools-leader-key " g")) #'sci-emacs-gramma-check)
(global-set-key (kbd (concat sci-emacs-tools-leader-key " o")) #'sci-emacs-wsl-open-external)
(global-set-key (kbd (concat sci-emacs-tools-leader-key " t")) #'sci-emacs-insert-date)


(provide 'init)
;;; init.el ends here
