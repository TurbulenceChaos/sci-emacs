;;; init-dashboard.el --- Dashboard config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))
(require 'dashboard)

(setq dashboard-banner-logo-title "Welcome to Sci-Emacs!")
(setq dashboard-footer-messages '("https://github.com/TurbulenceChaos/Sci-Emacs"))
(setq dashboard-items '((recents . 5)
                        (bookmarks . 5)
                        (projects . 5)))
(setq dashboard-display-icons-p t)
(setq dashboard-icon-type 'nerd-icons)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(dashboard-modify-heading-icons '((recents . "nf-oct-file")
				  (bookmarks . "nf-oct-book")
                                  (projects . "nf-oct-rocket")))
(setq dashboard-center-content t)
(dashboard-setup-startup-hook)


(provide 'init-dashboard)
;;; init-dashboard.el ends here
