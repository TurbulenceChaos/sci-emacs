;;; init-theme.el --- Theme config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'leuven-theme)
  (package-install 'leuven-theme))
(require 'leuven-theme)

(load-theme 'leuven :no-confirm)
(custom-set-faces
 '(line-number-current-line ((t (:inherit org-done :box nil :weight bold)))))


(provide 'init-theme)
;;; init-theme.el ends here
