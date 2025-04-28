;;; init-snippet.el --- Snippet Mathematica config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))
(require 'yasnippet)

(add-to-list 'yas-snippet-dirs (expand-file-name "snippet-collection/" user-emacs-directory))

(yas-global-mode 1)

(add-hook 'org-mode-hook (lambda ()
			   (setq-local electric-pair-inhibit-predicate
				       `(lambda (c)
					  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(provide 'init-snippet)
;;; init-snippet.el ends here
