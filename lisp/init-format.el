;;; init-format.el --- Format config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'apheleia)
  (package-install 'apheleia))

(apheleia-global-mode +1)


(provide 'init-format)
;;; init-format.el ends here
