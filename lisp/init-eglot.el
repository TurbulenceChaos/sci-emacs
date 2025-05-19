;;; init-eglot.el --- Eglot config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

(unless (package-installed-p 'eglot)
  (package-install 'eglot))


(provide 'init-eglot)
;;; init-eglot.el ends here
