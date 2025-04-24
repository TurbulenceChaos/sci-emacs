;;; init-theme.el --- Theme config
;;; Commentary:
;;; Code:

(package-install 'leuven-theme)
(load-theme 'leuven :no-confirm)

;; (custom-set-faces
;;  '(line-number-current-line ((t (:inherit line-number :background "blue" :foreground "white" :weight bold)))))

(provide 'init-theme)
;;; init-theme.el ends here
