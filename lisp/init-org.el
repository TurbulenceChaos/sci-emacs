;;; init-org.el --- Org-mode config
;;; Commentary:
;;; Code:

(setq org-startup-numerated t)

;; LaTeX
(require 'ox-latex)
(setq org-startup-with-latex-preview t)
(setq org-preview-latex-image-directory "tmp/ltximg/")

(add-to-list 'org-latex-packages-alist '("" "tikz" t))
(setf org-format-latex-header (concat "% xelatex\n" org-format-latex-header))
(setq org-format-latex-options (plist-put org-format-latex-options :scale 3))

(setq org-babel-default-header-args:latex '((:results . "graphics file")				
                                            (:imagemagick . "t")
                                            (:fit . "yes")
                                            (:iminoptions . "-density 300 -units pixelsperinch")
                                            (:imoutoptions . "-quality 100 -alpha remove")
                                            (:noweb . "yes")
                                            (:comments . "link")
                                            (:eval . "never-export")))

;; Emacs-jupyter config
(setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                     (:kernel . "python3")
                                                     (:session . "jupyter-python")
                                                     (:comments . "link")
                                                     (:eval . "never-export")))

(defalias 'wolfram-language-mode 'xah-wolfram-mode)

(setq org-babel-default-header-args:jupyter-Wolfram-Language '((:async . "yes")
                                                               (:kernel . "wolframlanguage14.1")
                                                               (:session . "jupyter-wolfram-language")
                                                               (:results . "value drawer")
                                                               (:display . "text")
                                                               (:comments . "link")
                                                               (:eval . "never-export")))

(setq org-babel-min-lines-for-block-output 1000)

(defun clean-wolfram-results ()
  "Clean up Wolfram Language results in org-mode.
Removes unwanted formatting while preserving necessary LaTeX formatting.
Keeps ': ' prefix only for lines starting with 'Out['."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward ":results:" nil t)
      (let ((start (point))
            (end (progn (search-forward ":end:" nil t)
                        (match-beginning 0))))
        (save-restriction
          (narrow-to-region start end)
          
          ;; First mark 'Out[' lines to preserve them
          (goto-char (point-min))
          (while (re-search-forward "^: Out\\[" nil t)
            (add-text-properties (line-beginning-position) (line-end-position)
                                '(preserve-prefix t)))
          
          ;; Remove ': ' at beginning of lines except those marked
          (goto-char (point-min))
          (while (re-search-forward "^: " nil t)
            (unless (get-text-property (line-beginning-position) 'preserve-prefix)
              (replace-match "" nil nil)))
          
          ;; Remove text properties we added
          (remove-text-properties (point-min) (point-max) '(preserve-prefix nil))
          
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

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)
   (python . t)
   (jupyter . t)))

;; Org-sliced-images config
(require 'org-sliced-images)

(setq org-sliced-images-round-image-height t)
(org-sliced-images-mode 1)

(add-hook 'org-mode-hook
	  '(lambda ()
	     (org-sliced-images-display-inline-images)
	     (save-buffer)))

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


(provide 'init-org)
;;; init-org.el ends here
