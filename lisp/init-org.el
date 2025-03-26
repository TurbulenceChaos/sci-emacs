;;; init-org.el --- Comprehensive Org-mode Configuration
;;; Commentary:
;;; This configuration file sets up advanced Org-mode functionality,
;;; including LaTeX preview, Babel language support, and image handling.

;;; Code:

;; Startup Behavior
;; Enable automatic numbering for org lists
(setq org-startup-numerated t)

;; LaTeX Configuration
;; Load LaTeX export functionality
(require 'ox-latex)

;; LaTeX Preview Settings
(setq org-startup-with-latex-preview t)  ; Enable LaTeX preview on startup
(setq org-format-latex-options (plist-put org-format-latex-options :scale 3))  ; Increase preview size
(setq org-preview-latex-image-directory "tmp/ltximg/")  ; Directory for storing LaTeX preview images
(add-to-list 'org-latex-packages-alist '("" "tikz" t))  ; Include TikZ package for LaTeX exports
(setf org-format-latex-header (concat "% xelatex\n" org-format-latex-header))  ; Specify XeLaTeX as the default LaTeX engine

;; Org-babel Configuration
;; Set minimum lines for block output (helps with large code blocks)
(setq org-babel-min-lines-for-block-output 1000)

;; Disable confirmation for code block execution (use with caution)
(setq org-confirm-babel-evaluate nil)

;; Configure supported languages for code execution within Org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)  ; Emacs Lisp
   (latex . t)       ; LaTeX
   (python . t)      ; Python
   (jupyter . t)))   ; Jupyter notebook integration

;; Default header arguments for LaTeX code blocks
(setq org-babel-default-header-args:latex 
      '((:results . "graphics file")     ; Output as graphics
        (:imagemagick . "t")             ; Use ImageMagick for processing
        (:fit . "yes")                   ; Fit image to content
        (:iminoptions . "-density 300 -units pixelsperinch")  ; High-resolution input
        (:imoutoptions . "-quality 100 -alpha remove")        ; High-quality output
        (:noweb . "yes")                 ; Enable code block references
        (:comments . "link")             ; Handle comments
        (:eval . "never-export")))       ; Prevent code evaluation during export

;; Default header arguments for Jupyter Python code blocks
(setq org-babel-default-header-args:jupyter-python
      '((:async . "yes")                 ; Run asynchronously
        (:kernel . "python3")            ; Use Python 3 kernel
        (:session . "jupyter-python")    ; Session name
        (:comments . "link")             ; Handle comments
        (:eval . "never-export")))       ; Prevent code evaluation during export

;; Wolfram Language Configuration
;; Alias for Wolfram Language mode
(defalias 'wolfram-language-mode 'xah-wolfram-mode)

;; Default header arguments for Jupyter Wolfram Language code blocks
(setq org-babel-default-header-args:jupyter-Wolfram-Language
      '((:async . "yes")                 ; Run asynchronously
        (:kernel . "wolframlanguage14.1")  ; Specific Wolfram Language kernel
        (:session . "jupyter-wolfram-language")  ; Session name
        (:results . "value drawer")      ; Result display style
        (:display . "text")              ; Display as text
        (:comments . "link")             ; Handle comments
        (:eval . "never-export")))       ; Prevent code evaluation during export

;; Custom function to clean Wolfram Language results
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

          ;; Preserve 'Out[' lines
          (goto-char (point-min))
          (while (re-search-forward "^: Out\\[" nil t)
            (add-text-properties (line-beginning-position) (line-end-position)
                                 '(preserve-prefix t)))

          ;; Remove ': ' at line beginnings
          (goto-char (point-min))
          (while (re-search-forward "^: " nil t)
            (unless (get-text-property (line-beginning-position) 'preserve-prefix)
              (replace-match "" nil nil)))

          ;; Clean up additional formatting
          (goto-char (point-min))
          (while (re-search-forward "^> " nil t)
            (replace-match "  " nil nil))

          ;; Remove single backslashes at line ends
          (goto-char (point-min))
          (while (re-search-forward "\\([^\\]\\)\\\\\\s-*$" nil t)
            (replace-match "\\1" nil nil))

          ;; Remove blank lines
          (goto-char (point-min))
          (while (re-search-forward "\n\\s-*\n" nil t)
            (replace-match "\n" nil nil))

          ;; Ensure a newline after :results:
          (goto-char (point-min))
          (unless (looking-at "\n")
            (insert "\n")))))))

;; Org-sliced-images Configuration
;; Load and configure org-sliced-images for better image handling
(require 'org-sliced-images)

;; Round image heights for consistent display
(setq org-sliced-images-round-image-height t)
(org-sliced-images-mode 1)

;; Hooks for automatic image and buffer management
(add-hook 'org-mode-hook
          (lambda ()
            (org-sliced-images-display-inline-images)
            (save-buffer)))

(add-hook 'org-babel-after-execute-hook
          (lambda ()
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
