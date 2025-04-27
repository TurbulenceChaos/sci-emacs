;;; init-latex.el --- LaTeX config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'auctex)
  (package-install 'auctex))
(require 'auctex)

(unless (package-installed-p 'auctex-latexmk)
  (package-install 'auctex-latexmk))
(require 'auctex-latexmk)

(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools))
(require 'pdf-tools)

(pdf-tools-install)

(setq TeX-parse-self t ; parse on load
      TeX-auto-save t  ; parse on save
      ;; Use hidden directories for AUCTeX files.
      TeX-auto-local ".auctex-auto"
      TeX-style-local ".auctex-style"
      TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex
      ;; Don't start the Emacs server when correlating sources.
      TeX-source-correlate-start-server nil
      ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
      TeX-electric-sub-and-superscript t
      ;; Just save, don't ask before each compilation.
      TeX-save-query nil)

(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

(setq TeX-view-program-selection '((output-pdf "PDF Tools")))

(with-eval-after-load 'latex 
  (define-key LaTeX-mode-map 
              (kbd "C-c C-g") 'pdf-sync-forward-search))


(provide 'init-latex)
;;; init-latex.el ends here
