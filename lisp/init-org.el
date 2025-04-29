;;; init-org.el --- Comprehensive Org-mode Configuration
;;; Commentary:
;;; This configuration file sets up advanced Org-mode functionality,
;;; including LaTeX preview, Babel language support, and image handling.

;;; Code:

(unless (package-installed-p 'org)
  (package-install 'org))
;; (require 'org)

;; org-basic
(setq org-startup-numerated t
      org-support-shift-select t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0
      org-babel-min-lines-for-block-output 1000
      org-startup-with-latex-preview t
      org-preview-latex-image-directory "tmp/ltximg/")
(require 'ox-latex)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 3))
(with-eval-after-load 'org-faces
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch :height 0.85))
(with-eval-after-load 'org
  (custom-set-faces
   `(line-number-current-line ((t (:inherit line-number
					    :foreground ,(face-attribute 'org-done :foreground nil t)
					    :background ,(face-attribute 'org-done :background nil t)
					    :box nil
					    :weight bold))))))

;; org-theme
(unless (package-installed-p 'org-modern)
  (package-install 'org-modern))
;; (require 'org-modern)

(setq org-modern-table nil)
(setq org-modern-block-fringe nil)
(global-org-modern-mode)

;; org-babel
(setq org-src-block-faces '(("emacs-lisp" (:background "#E5FFB8" :extend t)) ;; #EEE2FF, #E5FFB8, gray90
			    ("jupyter-python" (:background "thistle1" :extend t))
			    ("jupyter-Wolfram-Language" (:background "LightCyan1" :extend t))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)
   (python . t)
   (jupyter . t)))

;; latex-tikz
(add-to-list 'org-latex-packages-alist '("" "tikz" t))
(setq org-format-latex-header (concat "% xelatex\n" org-format-latex-header))
(setq org-babel-default-header-args:latex 
      '((:results . "graphics file")
        (:results . "value drawer")
        (:imagemagick . "t")
        (:fit . "yes")
        (:iminoptions . "-density 300 -units pixelsperinch")
        (:imoutoptions . "-quality 100 -alpha remove")
        (:noweb . "yes")
        (:comments . "link")
        (:eval . "never-export")))

;; jupyter-python
(setq org-babel-default-header-args:jupyter-python
      '((:async . "yes")
        (:kernel . "python3")
        (:session . "jupyter-python")
        (:results . "value drawer")
        (:comments . "link")
        (:eval . "never-export")))

;; jupyter-Wolfram-Language
(with-eval-after-load 'org
  (defalias 'wolfram-language-mode 'xah-wolfram-mode))

(setq org-babel-default-header-args:jupyter-Wolfram-Language
      '((:async . "yes")
        (:kernel . "wolframlanguage14.1")
        (:session . "jupyter-wolfram-language")
        (:results . "value drawer")
        (:display . "text")
        (:comments . "link")
        (:eval . "never-export")))

(defcustom wolfram-terminal-formula-type=latex t
  "A boolean option.  When set to t, wolfram-terminal-formula-type='latex';
when set to nil, wolfram-terminal-formula-type='image'."
  :type 'boolean
  :group 'wolfram-terminal-image)

(defun clean-jupyter-wolfram-language-results ()
  "Clean up jupyter-Wolfram-Language results."
  (let ((result-beg (org-babel-where-is-src-block-result)))
    (save-excursion
      (when (and result-beg
		 (goto-char result-beg))
	(let ((beg (re-search-forward "^:results:" nil t))
	      (end   (re-search-forward "^:end:" nil t)))
	  (save-restriction
	    (narrow-to-region beg end)
	    ;; Remove ': ' at beginning
	    (goto-char (point-min))
	    (while (re-search-forward "^: " nil t)
	      (replace-match "" nil nil))

	    ;; Change 'Out[number]' to ': Out[number]'
	    (goto-char (point-min))
	    (while (re-search-forward "^Out\\[\\([0-9]+\\)\\]" nil t)
	      (replace-match ": Out[\\1]" nil nil))

	    (when wolfram-terminal-formula-type=latex
	      (goto-char (point-min))
	      (let ((latex-beg 0) (latex-end 0))
		(while (setq latex-beg (re-search-forward "^\\\\begin{equation\\*}" nil t))
		  (setq latex-end (re-search-forward "^\\\\end{equation\\*}" nil t))
		  (narrow-to-region latex-beg latex-end)
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
		    (replace-match "\\1" nil nil)))))))))))

;; org-sliced-images
(unless (package-installed-p 'org-sliced-images)
  (package-install 'org-sliced-images))
;; (require 'org-sliced-images)

(setq org-sliced-images-round-image-height t)

(with-eval-after-load 'org-sliced-images
  (defun org-sliced-images-display-inline-images (&optional include-linked refresh beg end)
    "Display sliced inline images.

See the docstring of `org-display-inline-images' for more info. This advice is
an amalgamation of different versions of that function, with the goal of being
compatible with Emacs 28+."
    (interactive "P")
    (when (display-graphic-p)
      (when refresh
	(org-sliced-images-remove-inline-images beg end)
	(when (fboundp 'clear-image-cache) (clear-image-cache)))
      (let ((end (or end (point-max))))
	(org-with-point-at (or beg (point-min))
          (let* ((case-fold-search t)
		 (file-extension-re (image-file-name-regexp))
		 (link-abbrevs (mapcar #'car
                                       (append org-link-abbrev-alist-local
                                               org-link-abbrev-alist)))
		 ;; Check absolute, relative file names and explicit
		 ;; "file:" links.  Also check link abbreviations since
		 ;; some might expand to "file" links.
		 (file-types-re
                  (format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?\\(?:file\\|attachment\\):\\)"
                          (if (not link-abbrevs) ""
                            (concat "\\|" (regexp-opt link-abbrevs))))))
            (while (re-search-forward file-types-re end t)
              (let* ((link (org-element-lineage
                            (save-match-data (org-element-context))
                            '(link) t))
                     (linktype (org-element-property :type link))
                     (inner-start (match-beginning 1))
                     (path
                      (cond
                       ;; No link at point; no inline image.
                       ((not link) nil)
                       ;; File link without a description.  Also handle
                       ;; INCLUDE-LINKED here since it should have
                       ;; precedence over the next case.  I.e., if link
                       ;; contains filenames in both the path and the
                       ;; description, prioritize the path only when
                       ;; INCLUDE-LINKED is non-nil.
                       ((or (not (org-element-property :contents-begin link))
                            include-linked)
			(and (or (equal "file" linktype)
				 (equal "attachment" linktype))
                             (org-element-property :path link)))
                       ;; Link with a description.  Check if description
                       ;; is a filename.  Even if Org doesn't have syntax
                       ;; for those -- clickable image -- constructs, fake
                       ;; them, as in `org-export-insert-image-links'.
                       ((not inner-start) nil)
                       (t
			(org-with-point-at inner-start
                          (and (looking-at
				(if (char-equal ?< (char-after inner-start))
                                    org-link-angle-re
                                  org-link-plain-re))
                               ;; File name must fill the whole
                               ;; description.
                               (= (org-element-property :contents-end link)
                                  (match-end 0))
                               (progn
				 (setq linktype (match-string 1))
				 (match-string 2))))))))
		(when (and path (string-match-p file-extension-re path))
                  (let ((file (if (equal "attachment" linktype)
                                  (progn
                                    (require 'org-attach)
                                    (ignore-errors (org-attach-expand path)))
				(expand-file-name path))))
                    ;; Expand environment variables.
                    (when file (setq file (substitute-in-file-name file)))
                    (when (and file (file-exists-p file))
                      (let ((width (org-display-inline-image--width link))
                            ;; Support Emacs 30+ align in a sec
                            ;; (align (and (fboundp 'org-image--align)
                            ;;             (org-image--align link)))
                            (old (get-char-property-and-overlay
                                  (org-element-property :begin link)
                                  'org-image-overlay)))
			(if (and (car-safe old) refresh)
                            (image-flush (overlay-get (cdr old) 'display))
                          (let ((image (org-sliced-images--create-inline-image file width)))
                            (when image
                              (org-sliced-images--without-undo
                               (let* ((image-pixel-cons (image-size image t))
                                      (image-pixel-h (cdr image-pixel-cons))
                                      (font-height (default-font-height)))
				 ;; Round image height
				 (when org-sliced-images-round-image-height
                                   (setq image-pixel-h
					 (truncate (* (fround (/ image-pixel-h font-height 1.0)) font-height)))
                                   (setf (image-property image :height) image-pixel-h)
                                   (setf (image-property image :width) nil))
				 (let* ((image-line-h (max 1.0001 (/ image-pixel-h font-height 1.0001)))
					(y 0.0) (dy (/ image-line-h))
					(left-margin nil)
					(dummy-zone-start nil)
					(dummy-zone-end nil)
					(ovfam nil))
                                   (image-flush image)
                                   (org-with-point-at (org-element-property :begin link)
                                     (when (> (current-column) 0)
                                       (setq left-margin (current-column)))
                                     (while (< y 1.0)
                                       (let (slice-start slice-end)
					 (if (= y 0.0)
                                             ;; Overlay link
                                             (progn
                                               (setq slice-start (org-element-property :begin link))
                                               (setq slice-end (org-element-property :end link))
                                               (end-of-line)
                                               (delete-char 1)
                                               (insert (propertize "\n" 'line-height t)))
                                           (setq slice-start (line-beginning-position))
                                           (setq slice-end (1+ (line-beginning-position)))
                                           (if (and org-sliced-images-consume-dummies
                                                    (equal
                                                     (buffer-substring-no-properties
                                                      (line-beginning-position)
                                                      (line-end-position))
                                                     " "))
                                               ;; Consume next line as dummy
                                               (progn
						 (when left-margin
                                                   (put-text-property
                                                    slice-start
                                                    slice-end
                                                    'line-prefix
                                                    `(space :width ,left-margin)))
						 (put-text-property
                                                  (line-end-position) (1+ (line-end-position)) 'line-height t)
						 (forward-line))
                                             ;; Create dummy line
                                             (insert (if left-margin
							 (propertize " " 'line-prefix `(space :width ,left-margin))
                                                       " "))
                                             (insert (propertize "\n" 'line-height t)))
                                           (when (not dummy-zone-start)
                                             (setq dummy-zone-start slice-start))
                                           (setq dummy-zone-end (1+ slice-end)))
					 (push
                                          (org-sliced-images--make-inline-image-overlay
                                           slice-start
                                           slice-end
                                           (list (list 'slice 0 y 1.0 dy) image))
                                          ovfam))
                                       (setq y (+ y dy))))
                                   (setq end (+ end (* 2 (- (ceiling image-line-h) 1))))
                                   (push (make-overlay dummy-zone-start dummy-zone-end) ovfam)
                                   (push ovfam org-sliced-images--image-overlay-families)))))))))))))))))))

(defun my/org-sliced-images-display-inline-images ()
  "Remove org-sliced-images before displaying it."
  (interactive)
  (org-sliced-images-remove-inline-images)
  (org-sliced-images-display-inline-images))

(defalias 'org-remove-inline-images 'org-sliced-images-remove-inline-images)
(defalias 'org-toggle-inline-images 'org-sliced-images-toggle-inline-images)
(defalias 'org-display-inline-images 'my/org-sliced-images-display-inline-images)

(defun quiet-save-buffer ()
  "Save current buffer without message."
  (interactive)
  (let ((inhibit-message t))
    (save-buffer)))

(add-hook 'org-mode-hook
          (lambda ()
            (org-sliced-images-display-inline-images)
            (quiet-save-buffer)))

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
              (org-sliced-images-remove-inline-images)
              (quiet-save-buffer))))

;; Display inline images and latex fragments in org-babel result
;; https://github.com/doomemacs/doomemacs/blob/303dd28db808b42a2397c0f4b9fdd71e606026ff/modules/lang/org/config.el#L297
(defmacro +org-define-babel-result-display-fn (name action doc)
  "Define a function to display elements in org-babel result.
NAME is the function name suffix.
ACTION is the display function to call.
DOC is the docstring."
  `(defun ,(intern (format "+org-redisplay-%s-in-babel-result-h" name)) ()
     ,doc
     (unless (or
              ;; ...but not while Emacs is exporting an org buffer
              (bound-and-true-p org-export-current-backend)
              ;; ...and not while tangling org buffers
              (string-match-p "^ \\*temp" (buffer-name)))
       (save-excursion
         (let* ((beg (org-babel-where-is-src-block-result))
                (end (progn (goto-char beg) (forward-line) (org-babel-result-end))))
           (save-restriction
             (narrow-to-region (min beg end) (max beg end))
             ,action))))))

(+org-define-babel-result-display-fn
 "latex-fragments"
 (org-latex-preview)
 "Redisplay latex fragments after executing org-block.")

(+org-define-babel-result-display-fn
 "inline-images"
 (org-display-inline-images)
 "Redisplay inline images after executing org-block.")

(defun org-display-images-in-babel-result ()
  "Display org-sliced-images after executing org block."
  (when (org-babel-where-is-src-block-result)
    (let ((lang (org-element-property :language (org-element-at-point))))
      (when (string= lang "jupyter-Wolfram-Language")
	(clean-jupyter-wolfram-language-results)
	(when wolfram-terminal-formula-type=latex
	  (+org-redisplay-latex-fragments-in-babel-result-h))))
    (+org-redisplay-inline-images-in-babel-result-h)))

(add-hook 'org-babel-after-execute-hook #'org-display-images-in-babel-result)


(provide 'init-org)
;;; init-org.el ends here
