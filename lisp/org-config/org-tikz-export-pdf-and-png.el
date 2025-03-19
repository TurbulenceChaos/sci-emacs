;;; Export both PDF and PNG files when executing an org-tikz block.

;; 1. Define toggle variable (default: enabled)
(defvar org-tikz-export-pdf-and-png-enabled t
  "When non-nil, save both PDF and PNG files for LaTeX blocks.")

;; 2. Modified `org-tikz-export-pdf-and-png' function with toggle check
(defun org-tikz-export-pdf-and-png (orig-fun tex-file &rest args)
  "Save PDF only if `org-tikz-export-pdf-and-png-enabled' is non-nil."
  (let ((pdf-file (apply orig-fun tex-file args)))
    (when org-tikz-export-pdf-and-png-enabled ; Check toggle state
      (save-excursion
        (let ((info (org-babel-get-src-block-info)))
          (when info
            (let* ((params (nth 2 info))
                   (out-file (cdr (assq :file params))))
              (when out-file
                (let ((target-pdf (concat (file-name-sans-extension out-file) ".pdf")))
                  (copy-file pdf-file target-pdf t))))))))
    pdf-file))

;; 3. Define toggle command
(defun org-tikz-toggle-export-pdf-and-png ()
  "Toggle exporting both PDF and PNG files for org-tikz blocks."
  (interactive)
  (setq org-tikz-export-pdf-and-png-enabled (not org-tikz-export-pdf-and-png-enabled))
  (message "Exporting both PDF and PNG files is %s." (if org-tikz-export-pdf-and-png-enabled "ENABLED" "DISABLED")))

;; 4. Activate advice
(advice-add 'org-babel-latex-tex-to-pdf :around #'org-tikz-export-pdf-and-png)

(provide 'org-tikz-export-pdf-and-png)
