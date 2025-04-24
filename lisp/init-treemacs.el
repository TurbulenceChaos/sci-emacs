;;; init-treemacs.el --- Treemacs config
;;; Commentary:
;;; Code:

(package-install 'treemacs)
(package-install 'treemacs-evil)
(require 'treemacs-evil)
(package-install 'treemacs-nerd-icons)
(require 'treemacs-nerd-icons)
(treemacs-load-theme "nerd-icons")


(provide 'init-treemacs)
;;; init-treemacs.el ends here
