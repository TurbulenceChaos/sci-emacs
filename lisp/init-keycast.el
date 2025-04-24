;;; init-keycast.el --- Keycast config
;;; Commentary:
;;; Code:

(package-install 'keycast)

(define-minor-mode keycast-mode-line-mode
  "Show current command and its key binding in the mode line (fix for use with doom-modeline)."
  :global t
  (if keycast-mode-line-mode
      (add-hook 'pre-command-hook 'keycast--update t)
    (remove-hook 'pre-command-hook 'keycast--update)))
(add-to-list 'global-mode-string '("" keycast-mode-line))

(provide 'init-keycast)
;;; init-keycast.el ends here
