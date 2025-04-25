;;; init-evil.el --- Evil config
;;; Commentary:
;;; Code:

(setq evil-want-keybinding nil)
(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)

(unless (package-installed-p 'evil-escape)
  (package-install 'evil-escape))
(require 'evil-escape)

(unless (package-installed-p 'evil-commentary)
  (package-install 'evil-commentary))
(require 'evil-commentary)

(unless (package-installed-p 'evil-collection)
  (package-install 'evil-collection))
(require 'evil-collection)

(unless (package-installed-p 'evil-multiedit)
  (package-install 'evil-multiedit))
(require 'evil-multiedit)

(evil-multiedit-default-keybinds)
(define-key iedit-mode-keymap (kbd "<tab>") nil)

(evil-mode 1)
(evil-set-undo-system 'undo-redo)
(evil-commentary-mode)
(evil-escape-mode)
(evil-collection-init)

(defun save-and-kill-this-buffer()(interactive)(save-buffer)(kill-current-buffer))
(global-set-key [remap evil-quit] 'kill-buffer-and-window)
(global-set-key [remap evil-save-and-quit] 'save-and-kill-this-buffer)
(global-set-key [remap evil-save-and-close] 'save-and-kill-this-buffer)

(setq evil-escape-excluded-major-modes
      '(treemacs-mode magit-status-mode))
(push 'visual evil-escape-excluded-states)
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)

(define-key evil-normal-state-map (kbd "TAB") #'evil-jump-item)
(evil-define-key 'normal org-mode-map (kbd "C-j") 'electric-newline-and-maybe-indent)
(evil-define-key 'normal org-mode-map (kbd "C-k") 'kill-line)


(provide 'init-evil)
;;; init-evil.el ends here
