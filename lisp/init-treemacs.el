;;; init-treemacs.el --- Treemacs config
;;; Commentary:
;;; Code:

(use-package treemacs
  :defer t)

(use-package treemacs-evil
  :defer t)

(use-package treemacs-nerd-icons
  :defer t
  :config
  (treemacs-load-theme "nerd-icons"))


(provide 'init-treemacs)
;;; init-treemacs.el ends here
