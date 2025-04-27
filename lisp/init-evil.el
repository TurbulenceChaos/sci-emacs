;;; init-evil.el --- Evil config
;;; Commentary:
;;; Code:

(setq evil-want-keybinding nil)
(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)

(defun save-and-kill-this-buffer()(interactive)(save-buffer)(kill-current-buffer))
(global-set-key [remap evil-quit] 'kill-buffer-and-window)
(global-set-key [remap evil-save-and-quit] 'save-and-kill-this-buffer)
(global-set-key [remap evil-save-and-close] 'save-and-kill-this-buffer)
(define-key evil-normal-state-map (kbd "TAB") #'evil-jump-item)
(evil-define-key 'normal org-mode-map (kbd "C-j") 'electric-newline-and-maybe-indent)
(evil-define-key 'normal org-mode-map (kbd "C-k") 'kill-line)
(evil-set-undo-system 'undo-redo)
(evil-mode 1)

(unless (package-installed-p 'evil-commentary)
  (package-install 'evil-commentary))
(require 'evil-commentary)

(evil-commentary-mode)

(unless (package-installed-p 'evil-escape)
  (package-install 'evil-escape))
(require 'evil-escape)

(setq evil-escape-excluded-major-modes'(treemacs-mode magit-status-mode magit-revision-mode)
      evil-escape-key-sequence "jk"
      evil-escape-delay 0.2)
(push 'visual evil-escape-excluded-states)
(evil-escape-mode)

(unless (package-installed-p 'evil-collection)
  (package-install 'evil-collection))
(require 'evil-collection)

(evil-collection-init)

(unless (package-installed-p 'evil-multiedit)
  (package-install 'evil-multiedit))
(require 'evil-multiedit)

(evil-multiedit-default-keybinds)
(define-key iedit-mode-keymap (kbd "<tab>") nil)


(provide 'init-evil)
;;; init-evil.el ends here
