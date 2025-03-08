;; ----------------------------- Configurations ----------------------------- ;;
;; Org-mode
(add-to-list 'load-path "~/.emacs.d/lisp-site/org-mode/lisp")
(require 'org)

(add-hook 'org-mode-hook
  	(lambda ()
            (toggle-truncate-lines nil)
  	  (toggle-word-wrap nil)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)))

(setq org-babel-default-header-args:latex '((:results . "graphics file")
					    (:imagemagick . "t")
					    (:fit . "yes")
					    (:iminoptions . "-density 300 -units pixelsperinch")
					    (:imoutoptions . "-quality 100 -alpha remove")
					    (:noweb . "yes")
					    (:eval . "never-export")))

(add-to-list 'org-latex-packages-alist '("" "tikz" t))
(setf org-format-latex-header (concat "% xelatex\n" org-format-latex-header))
(setq org-format-latex-options (plist-put org-format-latex-options :scale 3))
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("emacs-lisp" "latex"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; ---------------------------------- lisp ---------------------------------- ;;
(add-to-list 'load-path "~/.emacs.d/lisp")
;; Comment divider
(require 'comment-divider)
;; Org babel latex save both pdf and png
(require 'org-babel-latex-save-pdf)

;; ------------------------------- lisp-site -------------------------------- ;;
;; Auctex
(add-to-list 'load-path "~/.emacs.d/lisp-site/auctex")
(require 'font-latex)
(require 'auctex)
(require 'tex)
(require 'latex)

;; Org-sliced-images
(add-to-list 'load-path "~/.emacs.d/lisp-site/org-sliced-images")
(require 'org-sliced-images)
(org-sliced-images-mode)

;; Which-key
(add-to-list 'load-path "~/.emacs.d/lisp-site/emacs-which-key")
(require 'which-key)
(which-key-mode)

;; Vertico
(add-to-list 'load-path "~/.emacs.d/lisp-site/compat")
(require 'compat)
(add-to-list 'load-path "~/.emacs.d/lisp-site/vertico")
(require 'vertico)
(vertico-mode)

;; Corfu
(add-to-list 'load-path "~/.emacs.d/lisp-site/corfu")
(require 'corfu)
(setq corfu-auto t
      corfu-auto-prefix 1)
(global-corfu-mode)
(add-to-list 'load-path "~/.emacs.d/lisp-site/corfu/extensions")
(require 'corfu-popupinfo)
(setq corfu-popupinfo-delay 0)
(corfu-popupinfo-mode)
(add-to-list 'load-path "~/.emacs.d/lisp-site/nerd-icons.el")
(require 'nerd-icons)
(add-to-list 'load-path "~/.emacs.d/lisp-site/nerd-icons-corfu")
(require 'nerd-icons-corfu)
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

;; Cape
(add-to-list 'load-path "~/.emacs.d/lisp-site/cape")
(require 'cape)
(add-to-list 'completion-at-point-functions #'cape-dict) ; sudo apt install ispell

;; Yasnippet
(add-to-list 'load-path "~/.emacs.d/lisp-site/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(add-to-list 'load-path "~/.emacs.d/lisp-site/yasnippet-snippets")
(require 'yasnippet-snippets)
(add-to-list 'load-path "~/.emacs.d/lisp-site/yasnippet-capf")
(require 'yasnippet-capf)
(add-to-list 'completion-at-point-functions #'yasnippet-capf)
(setopt corfu-on-exact-match 'show) ; https://github.com/elken/yasnippet-capf/issues/17

;; Orderless
(add-to-list 'load-path "~/.emacs.d/lisp-site/orderless")
(require 'orderless)
(setq completion-styles '(substring orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

;; Magit
(add-to-list 'load-path "~/.emacs.d/lisp-site/dash.el")
(require 'dash)
(add-to-list 'load-path "~/.emacs.d/lisp-site/transient/lisp")
(require 'transient)
(add-to-list 'load-path "~/.emacs.d/lisp-site/with-editor/lisp")
(require 'with-editor)
(add-to-list 'load-path "~/.emacs.d/lisp-site/llama")
(require 'llama)
(add-to-list 'load-path "~/.emacs.d/lisp-site/magit/lisp")
(require 'magit)

;; Diff-hl
(add-to-list 'load-path "~/.emacs.d/lisp-site/diff-hl")
(require 'diff-hl)
(global-diff-hl-mode)

;; Format
(add-to-list 'load-path "~/.emacs.d/lisp-site/inheritenv")
(require 'inheritenv)
(add-to-list 'load-path "~/.emacs.d/lisp-site/emacs-language-id")
(require 'language-id)
(add-to-list 'load-path "~/.emacs.d/lisp-site/emacs-format-all-the-code")
(require 'format-all)

;; Doom-modeline
(add-to-list 'load-path "~/.emacs.d/lisp-site/s.el")
(require 's)
(add-to-list 'load-path "~/.emacs.d/lisp-site/f.el")
(require 'f)
(add-to-list 'load-path "~/.emacs.d/lisp-site/shrink-path.el")
(require 'shrink-path)
(add-to-list 'load-path "~/.emacs.d/lisp-site/doom-modeline")
(require 'doom-modeline)
(doom-modeline-mode 1)

;; Treemacs
(add-to-list 'load-path "~/.emacs.d/lisp-site/ht.el")
(require 'ht)
(add-to-list 'load-path "~/.emacs.d/lisp-site/pfuture")
(require 'pfuture)
(add-to-list 'load-path "~/.emacs.d/lisp-site/hydra")
(require 'hydra)
(add-to-list 'load-path "~/.emacs.d/lisp-site/treemacs/src/elisp")
(require 'treemacs)
(require 'treemacs-file-management)
(add-to-list 'load-path "~/.emacs.d/lisp-site/treemacs-nerd-icons")
(require 'treemacs-nerd-icons)
(treemacs-load-theme "nerd-icons")

;; Smartparens
(add-to-list 'load-path "~/.emacs.d/lisp-site/smartparens/")
(require 'smartparens-config)
(smartparens-global-mode)

;; Undo-fu-session
(add-to-list 'load-path "~/.emacs.d/lisp-site/undo-fu-session")
(require 'undo-fu-session)
(undo-fu-session-global-mode)

