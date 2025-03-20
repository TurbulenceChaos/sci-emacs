;;; Completion configuration

;; Which-key-mode
(which-key-mode)

;; Icomplete-mode
(icomplete-vertical-mode)
(setq icomplete-compute-delay 0)
(setq icomplete-delay-completions-threshold 0)
(setq icomplete-max-delay-chars 0)
(setq icomplete-scroll t)
(setq icomplete-show-matches-on-no-input t)
(define-key icomplete-vertical-mode-minibuffer-map (kbd "TAB") 'icomplete-force-complete)
(define-key icomplete-vertical-mode-minibuffer-map (kbd "RET") 'icomplete-force-complete-and-exit)
(define-key icomplete-vertical-mode-minibuffer-map (kbd "SPC") 'self-insert-command)
(define-key icomplete-vertical-mode-minibuffer-map (kbd "C-j") 'exit-minibuffer)

;; Orderless
(add-to-list 'load-path "~/.emacs.d/lisp-site/orderless")
(require 'orderless)
(setq completion-styles '(substring orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

;; Completion-preview-mode
(add-hook 'completion-at-point-functions #'comint-filename-completion)
;;(add-hook 'completion-at-point-functions #'ispell-completion-at-point)
(global-completion-preview-mode 1)
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local completion-preview-commands
                        '(org-self-insert-command
                          completion-preview-complete))))
(define-key completion-preview-active-mode-map (kbd "M-n")  'completion-preview-next-candidate)
(define-key completion-preview-active-mode-map (kbd "M-p")  'completion-preview-prev-candidate)


(provide 'completion-config)
