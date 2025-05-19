;;; init-completion.el --- Completion config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(which-key-mode)

(setq icomplete-compute-delay 0
      icomplete-delay-completions-threshold 0
      icomplete-max-delay-chars 0
      icomplete-scroll t
      icomplete-show-matches-on-no-input t)
(icomplete-vertical-mode)
(define-key icomplete-vertical-mode-minibuffer-map (kbd "TAB") 'icomplete-force-complete)
(define-key icomplete-vertical-mode-minibuffer-map (kbd "RET") 'icomplete-force-complete-and-exit)
(define-key icomplete-vertical-mode-minibuffer-map (kbd "SPC") 'self-insert-command)
(define-key icomplete-vertical-mode-minibuffer-map (kbd "C-j") 'exit-minibuffer)

(unless (package-installed-p 'orderless)
  (package-install 'orderless))
(setq completion-styles '(substring orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(global-completion-preview-mode 1)
(add-hook 'org-mode-hook
	  (lambda ()
	    (setq-local completion-preview-commands
			'(org-self-insert-command
			  completion-preview-complete))))
(add-hook 'completion-at-point-functions #'comint-filename-completion)
(define-key completion-preview-active-mode-map (kbd "M-n") 'completion-preview-next-candidate)
(define-key completion-preview-active-mode-map (kbd "M-p") 'completion-preview-prev-candidate)


(provide 'init-completion)
;;; init-completion.el ends here
