;;; init-org.el --- Comprehensive Org-mode Configuration
;;; Commentary:
;;; This configuration file sets up advanced Org-mode functionality,
;;; including LaTeX preview, Babel language support, and image handling.

;;; Code:

;; Org-theme
(unless (package-installed-p 'org)
  (package-install 'org))
(require 'org)

;; Startup Behavior
;; Enable automatic numbering for org lists
(setq org-startup-numerated t)
(setq org-support-shift-select t)

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

(with-eval-after-load 'org-faces
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch :height 0.85))

(setq  org-src-block-faces '(("emacs-lisp" (:background "DarkSeaGreen1" :extend t))
			     ("jupyter-python" (:background "thistle1" :extend t))
			     ("jupyter-Wolfram-Language" (:background "LightCyan1" :extend t))))

;; https://emacs.stackexchange.com/questions/46529/configuring-prettify-symbols-mode
(defun org/configure-prettify-symbols-alist ()
  "Set prettify symbols alist."
  (setq prettify-symbols-alist
	'(("lambda" . "λ") ("\\lambda" . "λ")
          ("alpha" . "α") ("\\alpha" . "α")
          ("beta" . "β") ("\\beta" . "β")
          ("gamma" . "γ") ("\\gamma" . "γ")
          ("Gamma" . "Γ") ("\\Gamma" . "Γ")
          ("delta" . "δ") ("\\delta" . "δ")
          ("Delta" . "Δ") ("\\Delta" . "Δ")
          ("epsilon" . "ε") ("\\epsilon" . "ε")
          ("zeta" . "ζ") ("\\zeta" . "ζ")
          ("eta" . "η") ("\\eta" . "η")
          ("theta" . "θ") ("\\theta" . "θ")
	  ("mu" . "μ") ("\\mu" . "μ")
          ("nu" . "ν") ("\\nu" . "ν")
          ("pi" . "π") ("\\pi" . "π")
          ("Pi" . "Π") ("\\Pi" . "Π")
          ("phi" . "φ") ("\\phi" . "φ")
          ("Phi" . "Φ") ("\\Phi" . "Φ")
          ("sigma" . "σ") ("\\sigma" . "σ")
          ("Sigma" . "Σ") ("\\Sigma" . "Σ")
          ("tau" . "τ") ("\\tau" . "τ")
          ("xi" . "ξ") ("\\xi" . "ξ")
          ("psi" . "ψ") ("\\psi" . "ψ")
          ("Psi" . "Ψ") ("\\Psi" . "Ψ")
          ("omega" . "ω") ("\\omega" . "ω")
          ("Omega" . "Ω") ("\\Omega" . "Ω")))
  (prettify-symbols-mode 1))

(add-hook 'org-mode-hook #'org/configure-prettify-symbols-alist)
(add-hook 'org-mode-hook #'org-toggle-pretty-entities)

(unless (package-installed-p 'org-modern)
  (package-install 'org-modern))
(require 'org-modern)

(setq org-modern-table nil)
(setq org-modern-block-fringe nil)
(global-org-modern-mode)

(add-to-list 'load-path (expand-file-name "site-lisp/org-modern-indent" user-emacs-directory))
(require 'org-modern-indent)

(custom-set-faces
 '(org-modern-indent-bracket-line ((t (:inherit org-block-begin-line :underline nil :weight light)))))

;; https://www.reddit.com/r/emacs/comments/i9pfld/disable_orgprettyentities_on_the_current_line/
(defvar my/current-line '(0 . 0)
  "(start . end) of current line in current buffer")

(make-variable-buffer-local 'my/current-line)

(defun my/unhide-current-line (limit)
  "Font-lock function"
  (let ((start (max (point) (car my/current-line)))
        (end (min limit (cdr my/current-line))))
    (when (< start end)
      (remove-text-properties start end '(invisible t display "" composition ""))
      (goto-char limit)
      t)))

(defun my/refontify-on-linemove ()
  "Post-command-hook"
  (let* ((start (line-beginning-position))
         (end (line-beginning-position 2))
         (needs-update (not (equal start (car my/current-line)))))
    (setq my/current-line (cons start end))
    (when needs-update
      (font-lock-fontify-block 2)
      (if org-modern-block-fringe
	  (if (org-in-src-block-p)
	      (when (org-modern-indent-mode)
		(org-modern-indent-mode -1))
	    (progn (org-modern-indent-init)
		   (when (not (org-modern-indent-mode))
		     (org-modern-indent-mode 1))))))))

(defun my/org-unhighlight ()
  "Install"
  (font-lock-add-keywords nil '((my/unhide-current-line)) t)
  (add-hook 'post-command-hook #'my/refontify-on-linemove nil t))

(add-hook 'org-mode-hook #'my/org-unhighlight)

;; (advice-add 'org-babel-execute:jupyter-Wolfram-Language :before '(lambda (&rest _args) (org-modern-indent-mode -1)))

(add-hook 'org-babel-after-execute-hook
          (lambda ()
	    (if org-modern-block-fringe
		(progn
		  (org-modern-indent-init)
		  (when (org-modern-indent-mode)
		    (org-modern-indent-mode -1))))))

;; LaTeX Configuration
;; Load LaTeX export functionality
(require 'ox-latex)

;; LaTeX Preview Settings
(setq org-startup-with-latex-preview t)  ; Enable LaTeX preview on startup
(setq org-format-latex-options (plist-put org-format-latex-options :scale 3))  ; Increase preview size
(setq org-preview-latex-image-directory "tmp/ltximg/")  ; Directory for storing LaTeX preview images
(add-to-list 'org-latex-packages-alist '("" "tikz" t))  ; Include TikZ package for LaTeX exports
(setf org-format-latex-header (concat "% xelatex\n" org-format-latex-header))  ; Specify XeLaTeX as the default LaTeX engine

(unless (package-installed-p 'org-fragtog)
  (package-install 'org-fragtog))
(require 'org-fragtog)
;; (add-hook 'org-mode-hook 'org-fragtog-mode)

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
        (:results . "value drawer")      ; Result display style
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
        (:results . "value drawer")      ; Result display style
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
(defun clean-jupyter-wolfram-language-results ()
  "Clean up Wolfram Language results in org-mode."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "#+begin_src jupyter-Wolfram-Language" nil t)
      (let ((start (search-forward ":results:" nil t))
            (end   (search-forward ":end:" nil t)))
        (save-restriction
          (narrow-to-region start end)

          ;; Remove ': ' at beginning
          (goto-char (point-min))
          (while (re-search-forward "^: " nil t)
	    (replace-match "" nil nil))

          ;; Remove blank lines
          (goto-char (point-min))
          (while (re-search-forward "\n\\s-*\n" nil t)
            (replace-match "\n" nil nil))
          
	  ;; Remove '>' at beginning
          (goto-char (point-min))
          (while (re-search-forward "^> " nil t)
            (replace-match " " nil nil))

          ;; Remove '\' at end
          (goto-char (point-min))
          (while (re-search-forward "\\([^\\]\\)\\\\\\s-*$" nil t)
            (replace-match "\\1" nil nil))

	  ;; Change 'Out[]' to ': Out[]'
	  (goto-char (point-min))
          (while (re-search-forward "^Out" nil t)
	    (replace-match ": Out" nil nil)))))))

;; Org-images Configuration
(add-to-list 'load-path (expand-file-name "site-lisp/org-imgtog" user-emacs-directory))
(require 'org-imgtog)

(advice-add 'org-imgtog--show-img :after
            (lambda (&rest _)
              (let ((inhibit-message t))
                (save-buffer))))
(advice-add 'org-imgtog--hide-img-with-delay :after
            (lambda (&rest _)
              (let ((inhibit-message t))
                (save-buffer))))
;; (add-hook 'org-mode-hook '#org-imgtog-mode)

(unless (package-installed-p 'org-sliced-images)
  (package-install 'org-sliced-images))
(require 'org-sliced-images)

(defalias 'org-remove-inline-images 'org-sliced-images-remove-inline-images)
(defalias 'org-toggle-inline-images 'org-sliced-images-toggle-inline-images)
(defalias 'org-display-inline-images 'org-sliced-images-display-inline-images)
(setq org-sliced-images-round-image-height t)
(org-sliced-images-mode 1)

;; Hooks for automatic image and buffer management
(defun quiet-save-buffer ()
  "Save current buffer without message."
  (interactive)
  (let ((inhibit-message t))
    (save-buffer)))

(add-hook 'org-mode-hook
          (lambda ()
            (org-sliced-images-display-inline-images)
            (quiet-save-buffer)))

(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (org-sliced-images-remove-inline-images)
	    (clean-jupyter-wolfram-language-results)
            (org-sliced-images-display-inline-images)
            (org-latex-preview)))

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
              (org-sliced-images-remove-inline-images)
              (quiet-save-buffer))))


(provide 'init-org)
;;; init-org.el ends here
