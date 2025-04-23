;;; init-latex.el --- LaTeX config
;;; Commentary:
;;; Code:

(package-install 'auctex)
(package-install 'pdf-tools)

(setq TeX-view-program-selection '((output-pdf "PDF Tools")))

(provide 'init-latex)
;;; init-latex.el ends here
