;; ------------------------------- Appearance ------------------------------- ;;
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-face-attribute 'default nil :height 240) ;Font size
(global-display-line-numbers-mode)
(load-theme 'leuven t)

;; ----------------------------- Configuration ------------------------------ ;;
(setq make-backup-files nil)
(setq scroll-step            1
      scroll-conservatively  10000)
