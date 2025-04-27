;;; init-dashboard.el --- Dashboard config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))
;; (require 'dashboard)

(setq dashboard-banner-logo-title "Welcome to Sci-Emacs!"
      dashboard-footer-messages '("https://github.com/TurbulenceChaos/Sci-Emacs")
      dashboard-center-content t
      dashboard-icon-type 'nerd-icons
      dashboard-display-icons-p t
      dashboard-set-heading-icons t
      dashboard-set-file-icons t
      dashboard-modify-heading-icons '((recents . "nf-oct-file")
				       (bookmarks . "nf-oct-book")
				       (projects . "nf-oct-rocket"))
      dashboard-items '((recents . 5)
			(bookmarks . 5)
			(projects . 5)))
(dashboard-setup-startup-hook)


(provide 'init-dashboard)
;;; init-dashboard.el ends here
