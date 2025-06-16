;;; init-latex.el --- LaTeX config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'auctex)
  (package-install 'auctex))

(add-hook 'doc-view-mode-hook 'auto-revert-mode)


(provide 'init-latex)
;;; init-latex.el ends here
