;;; init-format.el --- Format config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'apheleia)
  (package-install 'apheleia))
;; (require 'apheleia)

;; https://github.com/lassik/emacs-format-all-the-code?tab=readme-ov-file#supported-languages

(apheleia-global-mode +1)


(provide 'init-format)
;;; init-format.el ends here
