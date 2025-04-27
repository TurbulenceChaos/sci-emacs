;;; init-theme.el --- Theme config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'leuven-theme)
  (package-install 'leuven-theme))
;; (require 'leuven-theme)

(load-theme 'leuven :no-confirm)


(provide 'init-theme)
;;; init-theme.el ends here
