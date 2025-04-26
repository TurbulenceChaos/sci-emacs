;;; init-theme.el --- Theme config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'leuven-theme)
  (package-install 'leuven-theme))
(require 'leuven-theme)

(load-theme 'leuven :no-confirm)

(with-eval-after-load 'org
  (custom-set-faces
   `(line-number-current-line ((t (:inherit line-number
                                  :foreground ,(face-attribute 'org-done :foreground nil t)
                                  :background ,(face-attribute 'org-done :background nil t)
                                  :box nil
                                  :weight bold))))))

(provide 'init-theme)
;;; init-theme.el ends here
