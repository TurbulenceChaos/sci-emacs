;; ------------------------------- Appearance ------------------------------- ;;
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-face-attribute 'default nil :height 240) ; Font size
(global-display-line-numbers-mode)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(global-prettify-symbols-mode 1)
(setq prettify-symbols-unprettify-at-point t)

;; ----------------------------- Configurations ----------------------------- ;;
(setq make-backup-files nil)
(setq ring-bell-function 'ignore)
(delete-selection-mode 1)
(setq scroll-step            1
      scroll-conservatively  10000)
