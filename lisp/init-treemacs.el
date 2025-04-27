;;; init-treemacs.el --- Treemacs config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'treemacs)
  (package-install 'treemacs))
;; (require 'treemacs)

(unless (package-installed-p 'treemacs-evil)
  (package-install 'treemacs-evil))
;; (require 'treemacs-evil)

(unless (package-installed-p 'treemacs-nerd-icons)
  (package-install 'treemacs-nerd-icons))
;; (require 'treemacs-nerd-icons)

(with-eval-after-load 'treemacs-nerd-icons
  (treemacs-load-theme "nerd-icons"))


(provide 'init-treemacs)
;;; init-treemacs.el ends here
