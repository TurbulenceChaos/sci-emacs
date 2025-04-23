;;; init-basic.el
;;; Comment:
;;; Code:

;; Disable UI elements for a minimal look
(tool-bar-mode -1)        ;; Disable toolbar
(scroll-bar-mode -1)      ;; Disable scrollbar
(menu-bar-mode -1)        ;; Disable menu bar

;; Set font size
(set-face-attribute 'default nil :height 240)

;; Enable useful global modes
(global-hl-line-mode t)             ;; Highlight current line
(global-display-line-numbers-mode)  ;; Show line numbers globally
(global-prettify-symbols-mode 1)    ;; Replace symbols like lambda → λ
(setq prettify-symbols-unprettify-at-point t) ;; Show original symbol on hover
(electric-pair-mode t)              ;; Auto-close brackets and quotes
(show-paren-mode 1)                 ;; Highlight matching parentheses
(delete-selection-mode t)           ;; Replace selection when typing
(global-subword-mode 1)             ;; Navigate camelCase words easily
(global-font-lock-mode 1)           ;; Enable syntax highlighting

;; Indentation settings
(setq tab-width 4
      indent-tabs-mode nil)         ;; Use spaces instead of tabs

;; File handling
(setq make-backup-files nil)        ;; Disable backup files
(global-auto-revert-mode t)         ;; Auto-reload files when changed externally

;; System behavior
(setq ring-bell-function 'ignore)   ;; Disable bell sound
(setq scroll-step 1
      scroll-conservatively 10000)  ;; Smooth scrolling


(provide 'init-basic)
;;; init-basic.el ends here.
