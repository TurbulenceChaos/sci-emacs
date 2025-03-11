;;; Undo configuration

(add-to-list 'load-path "~/.emacs.d/lisp-site/undo-fu-session")
(require 'undo-fu-session)

(undo-fu-session-global-mode)

(provide 'undo-config)
