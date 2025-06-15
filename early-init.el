;;; early-init.el --- Emacs 30.1 pre-initialisation config
;;; Commentary:
;;; Code:

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))


(provide 'early-init)
;;; early-init.el ends here
