;;; early-init.el --- Emacs 30.1 pre-initialisation config
;;; Commentary:
;;; Code:

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq byte-compile-warnings nil)
(setq native-comp-async-report-warnings-errors nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
;; (setq package-install-upgrade-built-in t)

(unless (member "Symbols Nerd Font Mono" (font-family-list))
  (nerd-icons-install-fonts t))

(provide 'early-init)
;;; early-init.el ends here
