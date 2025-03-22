;;; Org-sliced-images-config

(add-to-list 'load-path "~/.emacs.d/lisp-site/org-sliced-images")
(require 'org-sliced-images)

(setq org-sliced-images-round-image-height t)
(org-sliced-images-mode 1)

(defalias 'org-remove-inline-images 'org-sliced-images-remove-inline-images)
(defalias 'org-toggle-inline-images 'org-sliced-images-toggle-inline-images)
(defalias 'org-display-inline-images 'org-sliced-images-display-inline-images)

(add-hook 'org-mode-hook
	  '(lambda ()
	     (org-sliced-images-display-inline-images)
	     (save-buffer)))

(defun clean-wolfram-results ()
  "Clean up Wolfram Language results in org-mode.
Handles line prefixes, unwanted whitespace in equations, and trailing backslashes."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward ":results:" nil t)
      (let ((start (point))
            (end (progn (search-forward ":end:" nil t)
                        (match-beginning 0))))
        (save-restriction
          (narrow-to-region start end)
          
          ;; Remove ': ' at beginning of lines
          (goto-char (point-min))
          (while (re-search-forward "^: " nil t)
            (replace-match "" nil nil))
          
          ;; Remove '> ' at beginning of lines 
          (goto-char (point-min))
          (while (re-search-forward "^> " nil t)
            (replace-match "  " nil nil))
            
          ;; Remove trailing backslashes at the end of lines
          (goto-char (point-min))
          (while (re-search-forward "\\\\\\s-*$" nil t)
            (replace-match "" nil nil))
            
          ;; Remove blank lines
          (goto-char (point-min))
          (while (re-search-forward "\n\\s-*\n" nil t)
            (replace-match "\n" nil nil))
            
          ;; Create a new line after :results: if needed
          (goto-char (point-min))
          (unless (looking-at "\n")
            (insert "\n")))))))

(add-hook 'org-babel-after-execute-hook
          '(lambda ()
             (org-sliced-images-remove-inline-images)
             (clean-wolfram-results)
             (call-interactively 'org-latex-preview)
             (org-sliced-images-display-inline-images)))

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
              (org-sliced-images-remove-inline-images)
	      (save-buffer))))


(provide 'org-sliced-images-config)
