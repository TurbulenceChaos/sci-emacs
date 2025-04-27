;;; init-window.el --- Window config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'ace-window)
  (package-install 'ace-window))
;; (require 'ace-window)

(global-set-key [remap other-window] #'ace-window)

(unless (package-installed-p 'switch-window)
  (package-install 'switch-window))
;; (require 'switch-window)

(global-set-key [remap other-window] #'switch-window)

(custom-set-faces
 '(aw-leading-char-face ((t (:foreground "red" :weight normal :height 2.5)))))


(provide 'init-window)
;;; init-window.el ends here
