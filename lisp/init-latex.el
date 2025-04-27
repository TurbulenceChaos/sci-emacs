;;; init-latex.el --- LaTeX config
;;; Commentary:
;;; Code:

(use-package auctex
  :defer t
  :config
  (setq TeX-parse-self t
	TeX-auto-save t
	TeX-auto-local ".auctex-auto"
	TeX-style-local ".auctex-style"
	TeX-source-correlate-mode t
	TeX-source-correlate-method 'synctex
	TeX-source-correlate-start-server nil
	TeX-electric-sub-and-superscript t
	TeX-save-query nil))

(use-package auctex-latexmk
  :defer t)

(use-package pdf-tools
  :defer t
  :after latex
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :bind (:map LaTeX-mode-map
              ("C-c C-g" . pdf-sync-forward-search))
  :init
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  :config
  (pdf-tools-install)
  (require 'pdf-sync))


(provide 'init-latex)
;;; init-latex.el ends here
