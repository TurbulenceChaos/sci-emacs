;;; init-evil.el --- Evil config
;;; Commentary:
;;; Code:

(package-install 'evil)
(package-install 'evil-escape)
(package-install 'evil-commentary)

(evil-mode 1)
(evil-set-undo-system 'undo-redo)
(evil-commentary-mode)
(evil-escape-mode)

(defun save-and-kill-this-buffer()(interactive)(save-buffer)(kill-current-buffer))
(global-set-key [remap evil-quit] 'kill-buffer-and-window)
(global-set-key [remap evil-save-and-quit] 'save-and-kill-this-buffer)
(global-set-key [remap evil-save-and-close] 'save-and-kill-this-buffer)
(setq-default evil-escape-key-sequence "jk")

(provide 'init-evil)
;;; init-evil.el ends here
