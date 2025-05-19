;;; init-basic.el --- Basic config
;;; Comment:
;;; Code:

(set-face-attribute 'default nil :height 240)
(global-hl-line-mode t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-src-mode-hook #'display-line-numbers-mode)
(global-prettify-symbols-mode 1)
(setq prettify-symbols-unprettify-at-point t)
(electric-pair-mode t)
(show-paren-mode 1)
(delete-selection-mode t)
(setq make-backup-files nil
      auto-save-default nil)
(global-auto-revert-mode t)
(setq ring-bell-function 'ignore)
(setq hscroll-step 1
      scroll-conservatively 10)


(provide 'init-basic)
;;; init-basic.el ends here.
