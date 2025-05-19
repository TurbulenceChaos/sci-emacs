;;; init-undo.el --- Undo config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'undo-fu)
  (package-install 'undo-fu))

(unless (package-installed-p 'undo-fu-session)
  (package-install 'undo-fu-session))

(setq undo-fu-session-compression nil)
(undo-fu-session-global-mode)

(unless (package-installed-p 'vundo)
  (package-install 'vundo))


(provide 'init-undo)
;;; init-undo.el ends here
