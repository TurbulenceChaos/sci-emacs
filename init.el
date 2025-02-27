;; ------------------------------- Functions -------------------------------- ;;
;; Comment divider
(defun comment-divider (text)
  "Create a comment divider of exactly 80 characters based on TEXT.
The format is: ;; --- <TEXT> --- ;;."
  (interactive "sEnter text for divider: ")
  (let* ((total-length 80) ; desired length
         (comment-prefix ";; ") ; Comment prefix (start)
         (comment-suffix " ;;") ; Comment suffix (end, includes leading space)
         (prefix-length (length comment-prefix))
         (suffix-length (length comment-suffix))
         (text-with-spaces (concat " " text " ")) ; Add spaces around the input text
         (text-length (length text-with-spaces))
         (dash-length (- total-length prefix-length suffix-length text-length)) ; Remaining space for dashes
         (half-dash-length (/ dash-length 2)) ; Divide remaining space equally
         (divider (concat (make-string half-dash-length ?-)
                          text-with-spaces
                          (make-string (- dash-length half-dash-length) ?-))))
    (insert comment-prefix divider comment-suffix)))

;; -------------------------------- Packages -------------------------------- ;;
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
(add-to-list 'load-path "~/.emacs.d/lisp-site/corfu")
(require 'corfu)
;; (setq corfu-auto t)
(global-corfu-mode)
(add-to-list 'load-path "~/.emacs.d/lisp-site/corfu/extensions")
(require 'corfu-popupinfo)
(setq corfu-popupinfo-delay 0)
(corfu-popupinfo-mode)

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
(add-to-list 'load-path "~/.emacs.d/lisp-site/treemacs/src/elisp")
(require 'treemacs)
(add-to-list 'load-path "~/.emacs.d/lisp-site/treemacs-nerd-icons")
(require 'treemacs-nerd-icons)
(treemacs-load-theme "nerd-icons")

;; To-do: file path completion
