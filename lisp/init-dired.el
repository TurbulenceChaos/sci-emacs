;;; init-dired.el --- Dired config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'nerd-icons-dired)
  (package-install 'nerd-icons-dired))

(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)


(provide 'init-dired)
;;; init-dired.el ends here
