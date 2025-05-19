;;; init-evil.el --- Evil config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'evil)
  (package-install 'evil))

(with-eval-after-load 'evil
  (evil-set-undo-system 'undo-redo))

(unless (package-installed-p 'evil-org)
  (package-install 'evil-org))


(provide 'init-evil)
;;; init-evil.el ends here
