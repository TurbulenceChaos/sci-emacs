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

(defun replace-colon-brackets ()
  "Replace ': [[' at the beginning of a line with '[[' in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^: \\[\\[file:" nil t)
      (replace-match "[[file:" nil nil))))

(add-hook 'org-babel-after-execute-hook
	  '(lambda ()
	     (replace-colon-brackets)
	     (org-sliced-images-remove-inline-images)
	     (org-sliced-images-display-inline-images)))

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
              (org-sliced-images-remove-inline-images)
	      (save-buffer))))


(provide 'org-sliced-images-config)
