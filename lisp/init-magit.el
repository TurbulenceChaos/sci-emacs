;;; init-magit.el --- Magit config
;;; Commentary:
;;; Code:

(use-package magit
  :defer t)

(use-package diff-hl
  :defer t
  :config
  (global-diff-hl-mode))


(provide 'init-magit)
;;; init-magit.el ends here
