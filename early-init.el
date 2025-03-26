;;; early-init.el --- Emacs 30.1 pre-initialisation config
;;; Commentary:
;;; Code:

(setq byte-compile-warnings nil)
(setq native-comp-async-report-warnings-errors nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))


(provide 'early-init)
;;; early-init.el ends here