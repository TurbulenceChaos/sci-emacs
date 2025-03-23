;;; Org-sliced-images-config

(add-to-list 'load-path "~/.emacs.d/lisp-site/org-sliced-images")
(require 'org-sliced-images)

(setq org-sliced-images-round-image-height t)
(org-sliced-images-mode 1)

(defalias 'org-remove-inline-images 'org-sliced-images-remove-inline-images)
(defalias 'org-toggle-inline-images 'org-sliced-images-toggle-inline-images)
(defalias 'org-display-inline-images 'org-sliced-images-display-inline-images)

(setq org-babel-min-lines-for-block-output 1000)

(defun clean-wolfram-results ()
  "Clean up Wolfram Language results in org-mode.
Removes unwanted formatting while preserving necessary LaTeX formatting."
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
            
          ;; Remove SINGLE backslashes at end of lines (not double backslashes)
          (goto-char (point-min))
          (while (re-search-forward "\\([^\\]\\)\\\\\\s-*$" nil t)
            (replace-match "\\1" nil nil))
            
          ;; Remove blank lines
          (goto-char (point-min))
          (while (re-search-forward "\n\\s-*\n" nil t)
            (replace-match "\n" nil nil))
            
          ;; Create a new line after :results: if needed
          (goto-char (point-min))
          (unless (looking-at "\n")
            (insert "\n")))))))

(defun org-sliced-images-display-inline-images-setup ()
  "Set up org-mode with both LaTeX previews and sliced images."
  ;; First load images
  (org-sliced-images-display-inline-images)
  ;; Then process all LaTeX fragments
  (org-latex-preview)
  ;; Finally, find and fix any LaTeX fragments that follow file links
  (org-with-point-at (point-min)
    (while (re-search-forward "\\[\\[file:" nil t)
      (let ((link-end (save-excursion (search-forward "]]" nil t))))
        (when link-end
          (goto-char link-end)
          (org-next-visible-heading 1)
          (org-latex-preview)))))
  (save-buffer))

(add-hook 'org-mode-hook 'org-sliced-images-display-inline-images-setup)

(add-hook 'org-babel-after-execute-hook
          '(lambda ()
             (org-sliced-images-remove-inline-images)
             (clean-wolfram-results)
             (org-sliced-images-display-inline-images)
             (org-latex-preview)))

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
              (org-sliced-images-remove-inline-images)
	      (save-buffer))))


(provide 'org-sliced-images-config)
