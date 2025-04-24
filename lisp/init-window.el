;;; init-window.el --- Window config
;;; Commentary:
;;; Code:

(package-install 'ace-window)
(package-install 'switch-window)
(global-set-key [remap other-window] #'switch-window)
(global-set-key [remap other-window] #'ace-window)
(custom-set-faces
   '(aw-leading-char-face ((t (:foreground "red" :weight normal :height 2.5)))))


(provide 'init-window)
;;; init-window.el ends here
