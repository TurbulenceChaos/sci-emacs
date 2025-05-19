;;; init-evil.el --- Evil config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'evil)
  (package-install 'evil))

(with-eval-after-load 'evil
  (evil-set-undo-system 'undo-redo)
  (add-to-list 'evil-emacs-state-modes 'image-mode))

(unless (package-installed-p 'evil-org)
  (package-install 'evil-org))

(add-hook 'org-mode-hook 'evil-org-mode)

(provide 'init-evil)
;;; init-evil.el ends here
