;;; init-evil.el --- Evil config
;;; Commentary:
;;; Code:

(package-install 'evil)
(package-install 'evil-escape)
(package-install 'evil-commentary)
(package-install 'evil-collection)

(setq evil-want-keybinding nil)
(evil-mode 1)
(evil-set-undo-system 'undo-redo)
(evil-commentary-mode)
(evil-escape-mode)
(evil-collection-init)

(defun save-and-kill-this-buffer()(interactive)(save-buffer)(kill-current-buffer))
(global-set-key [remap evil-quit] 'kill-buffer-and-window)
(global-set-key [remap evil-save-and-quit] 'save-and-kill-this-buffer)
(global-set-key [remap evil-save-and-close] 'save-and-kill-this-buffer)
(setq-default evil-escape-key-sequence "jk")
(define-key evil-normal-state-map (kbd "TAB") #'evil-jump-item)
(evil-define-key 'normal org-mode-map (kbd "C-j") 'electric-newline-and-maybe-indent)
(evil-define-key 'normal org-mode-map (kbd "C-k") 'kill-line)


(provide 'init-evil)
;;; init-evil.el ends here
