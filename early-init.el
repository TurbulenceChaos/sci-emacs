;; ------------------------------- Appearance ------------------------------- ;;
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(tool-bar-mode -1)

(scroll-bar-mode -1)

(menu-bar-mode -1)

(global-display-line-numbers-mode)

(set-face-attribute 'default nil :height 240) ;Font size

;; ----------------------------- Configurations ----------------------------- ;;
(setq make-backup-files nil)

(setq ring-bell-function 'ignore)

(setq scroll-step            1
      scroll-conservatively  10000)

(delete-selection-mode 1)

(which-key-mode)

(add-hook 'completion-at-point-functions #'comint-filename-completion)
