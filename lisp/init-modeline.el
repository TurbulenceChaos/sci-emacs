;;; init-modeline.el --- Modeline config
;;; Commentary:
;;; Code:

(package-install 'doom-modeline)

(unless (member "Symbols Nerd Font Mono" (font-family-list))
  (nerd-icons-install-fonts t))

(doom-modeline-mode 1)


(provide 'init-modeline)
;;; init-modeline.el ends here
