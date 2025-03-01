;; ----------------------------- Configurations ----------------------------- ;;
;; Org-mode
(add-to-list 'load-path "~/.emacs.d/lisp-site/org-mode/lisp")
(require 'org)

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

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("emacs-lisp" "latex"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-id-locations-file "~/.emacs.d/.cache/")

;; File path completion: https://emacs.stackexchange.com/questions/79845/completion-at-point-functions-and-filesystem-path-completion
(add-hook 'completion-at-point-functions #'comint-filename-completion)

;; Delete whitespace when saving files
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Global prettify symbols mode
(global-prettify-symbols-mode)

;; ---------------------------------- lisp ---------------------------------- ;;
(add-to-list 'load-path "~/.emacs.d/lisp")
;; Comment divider
(require 'comment-divider)
;; Org babel latex save both pdf and png
(require 'org-babel-latex-save-pdf)

;; ------------------------------- lisp-site -------------------------------- ;;
;; Atom-one-dark theme
(add-to-list 'load-path "~/.emacs.d/lisp-site/atom-one-dark-theme")
(require 'atom-one-dark-theme)
(load-theme 'atom-one-dark t)

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
;; (add-to-list 'load-path "~/.emacs.d/lisp-site/corfu")
;; (require 'corfu)
;; (setq corfu-auto t)
;; (global-corfu-mode)
;; (add-to-list 'load-path "~/.emacs.d/lisp-site/corfu/extensions")
;; (require 'corfu-popupinfo)
;; (setq corfu-popupinfo-delay 0)
;; (corfu-popupinfo-mode)

;; Orderless
(add-to-list 'load-path "~/.emacs.d/lisp-site/orderless")
(require 'orderless)
(setq completion-styles '(orderless basic)
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
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/lisp-site/magit/Documentation/"))

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
(add-to-list 'load-path "~/.emacs.d/lisp-site/nerd-icons.el")
(require 'nerd-icons)
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

;; Org-sliced-images
(add-to-list 'load-path "~/.emacs.d/lisp-site/org-sliced-images")
(require 'org-sliced-images)
(org-sliced-images-mode)

;; Auctex
(add-to-list 'load-path "~/.emacs.d/lisp-site/auctex")
(require 'font-latex)
(require 'auctex)
(require 'tex)
(require 'latex)

;; Smartparens
(add-to-list 'load-path "~/.emacs.d/lisp-site/smartparens/")
(require 'smartparens-config)
(smartparens-global-mode)
;;(smartparens-global-strict-mode)

;; Lsp-bridge
(add-to-list 'load-path "~/.emacs.d/lisp-site/markdown-mode")
(require 'markdown-mode)
(add-to-list 'load-path "~/.emacs.d/lisp-site/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(add-to-list 'load-path "~/.emacs.d/lisp-site/yasnippet-snippets")
(require 'yasnippet-snippets)
(add-to-list 'load-path "~/.emacs.d/lisp-site/lsp-bridge")
(require 'lsp-bridge)
(global-lsp-bridge-mode)
(setq lsp-bridge-enable-org-babel t)
