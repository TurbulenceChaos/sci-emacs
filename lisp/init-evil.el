;;; init-evil.el --- Evil config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)

(evil-set-undo-system 'undo-redo)


(provide 'init-evil)
;;; init-evil.el ends here
