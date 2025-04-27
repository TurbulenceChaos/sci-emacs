;;; init-undo.el --- Undo config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'undo-fu-session)
  (package-install 'undo-fu-session))
;; (require 'undo-fu-session)

(undo-fu-session-global-mode)

(unless (package-installed-p 'vundo)
  (package-install 'vundo))
;; (require 'vundo)


(provide 'init-undo)
;;; init-undo.el ends here
