;;; init-magit.el --- Magit config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'magit)
  (package-install 'magit))
(require 'magit)

(unless (package-installed-p 'diff-hl)
  (package-install 'diff-hl))
(require 'diff-hl)

(global-diff-hl-mode)


(provide 'init-magit)
;;; init-magit.el ends here
