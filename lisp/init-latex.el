;;; init-latex.el --- LaTeX config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'auctex)
  (package-install 'auctex))
;; (require 'auctex)

(unless (package-installed-p 'auctex-latexmk)
  (package-install 'auctex-latexmk))
;; (require 'auctex-latexmk)

(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools))
;; (require 'pdf-tools)

(pdf-tools-install)

(setq TeX-parse-self t
      TeX-auto-save t
      TeX-auto-local ".auctex-auto"
      TeX-style-local ".auctex-style"
      TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex
      TeX-source-correlate-start-server nil
      TeX-electric-sub-and-superscript t
      TeX-save-query nil
      TeX-view-program-selection '((output-pdf "PDF Tools")))

(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

(with-eval-after-load 'latex 
  (define-key LaTeX-mode-map 
              (kbd "C-c C-g") 'pdf-sync-forward-search))


(provide 'init-latex)
;;; init-latex.el ends here
