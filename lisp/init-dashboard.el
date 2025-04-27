;;; init-dashboard.el --- Dashboard config
;;; Commentary:
;;; Code:

(setq dashboard-banner-logo-title "Emacs for science and mathematics!"
      dashboard-footer-messages '("https://github.com/TurbulenceChaos/Sci-Emacs")
      dashboard-startup-banner (expand-file-name "Sci-Emacs.png" user-emacs-directory)
      dashboard-center-content t
      dashboard-items '((recents . 5)
			(bookmarks . 5)
			(projects . 5))
      dashboard-icon-type 'nerd-icons
      dashboard-display-icons-p t
      dashboard-set-heading-icons t
      dashboard-set-file-icons t
      dashboard-projects-backend 'project-el)

(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))
(require 'dashboard)

(dashboard-setup-startup-hook)

(setq dashboard-footer-icon (nerd-icons-octicon "nf-oct-mark_github"
						:height 1.1
                                                :face 'font-lock-keyword-face))



(provide 'init-dashboard)
;;; init-dashboard.el ends here
