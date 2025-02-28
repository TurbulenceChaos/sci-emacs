;; 1. Define toggle variable (default: enabled)
(defvar org-babel-save-pdf-enabled t
  "When non-nil, save both PDF and PNG files for LaTeX blocks.")

;; 2. Modified PDF-saving function with toggle check
(defun org-babel-latex-save-pdf (orig-fun tex-file &rest args)
  "Save PDF only if `org-babel-save-pdf-enabled' is non-nil."
  (let ((pdf-file (apply orig-fun tex-file args)))
    (when org-babel-save-pdf-enabled ; Check toggle state
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
(defun org-babel-toggle-save-pdf ()
  "Toggle saving of PDF files for LaTeX blocks."
  (interactive)
  (setq org-babel-save-pdf-enabled (not org-babel-save-pdf-enabled))
  (message "PDF saving %s." (if org-babel-save-pdf-enabled "ENABLED" "DISABLED")))

;; 4. Activate advice
(advice-add 'org-babel-latex-tex-to-pdf :around #'org-babel-latex-save-pdf)

(provide 'org-babel-latex-save-pdf)
