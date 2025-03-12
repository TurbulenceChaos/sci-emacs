(add-to-list 'load-path "~/.emacs.d/lisp-site/xah-wolfram-mode")
(require 'xah-wolfram-mode)

(defalias 'wolfram-language-mode 'xah-wolfram-mode)

(defun replace-colon-brackets ()
  "Replace ': [[' at the beginning of a line with '[[' in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^: \\[\\[" nil t)
      (replace-match "[[" nil nil))))

(add-hook 'org-babel-after-execute-hook 'replace-colon-brackets)

(provide 'wolfram-config)
