;; ---------------------------------- lisp ---------------------------------- ;;
(add-to-list 'load-path "~/.emacs.d/lisp")
;; Comment divider
(require 'comment-divider)

;; Org babel latex save both pdf and png
(require 'org-babel-latex-save-pdf)

;; ------------------------------- lisp-site -------------------------------- ;;
;; Org-mode
(add-to-list 'load-path "~/.emacs.d/lisp-site/org-mode/lisp")
(require 'org)
(require 'org-config)

;; Org-sliced-images
(add-to-list 'load-path "~/.emacs.d/lisp-site/org-sliced-images")
(require 'org-sliced-images)
(org-sliced-images-mode)

;; Vertico
(add-to-list 'load-path "~/.emacs.d/lisp-site/compat")
(require 'compat)
(add-to-list 'load-path "~/.emacs.d/lisp-site/vertico")
(require 'vertico)
(vertico-mode)

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

;; Undo-fu-session
(add-to-list 'load-path "~/.emacs.d/lisp-site/undo-fu-session")
(require 'undo-fu-session)
(undo-fu-session-global-mode)
