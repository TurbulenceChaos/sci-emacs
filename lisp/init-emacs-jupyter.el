;;; init-emacs-jupyter.el --- Emacs-jupyter config
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :defer t)

(use-package jupyter
  :defer t
  :config
  ;; https://github.com/emacs-jupyter/jupyter/pull/582
  (with-eval-after-load 'jupyter-org-client
    (defun jupyter-org--set-src-block-cache ()
      "Set the src-block cache.
If set successfully or if `point' is already inside the cached
source block, return non-nil.  Otherwise, when `point' is not
inside a Jupyter src-block, return nil."
      (unless jupyter-org--src-block-cache
	(setq jupyter-org--src-block-cache
              (list 'invalid nil (make-marker)
                    (let ((end (make-marker)))
                      ;; Move the end marker when text is inserted
                      (set-marker-insertion-type end t)
                      end))))
      (if (org-in-src-block-p 'inside)
	  (or (jupyter-org--at-cached-src-block-p)
              (when-let* ((el (org-element-at-point))
			  (info (and (eq (org-element-type el) 'src-block)
                                     (org-babel-jupyter-language-p
                                      (org-element-property :language el))
                                     (org-babel-get-src-block-info t el)))
			  (params (nth 2 info)))
		(when (eq (car jupyter-org--src-block-cache) 'invalid)
		  (pop jupyter-org--src-block-cache))
		(pcase-let (((and cache `(,_ ,beg ,end))
                             jupyter-org--src-block-cache))
		  (setcar cache params)
		  (save-excursion
                    (goto-char (org-element-property :post-affiliated el))
                    (move-marker beg (line-beginning-position 2))
                    (goto-char (org-element-property :end el))
                    (skip-chars-backward "\r\n")
                    (move-marker end (line-beginning-position))))
		t))
	;; Invalidate cache when going outside of a source block.  This
	;; way if the language of the block changes we don't end up using
	;; the cache since it is only used for Jupyter blocks.
	(pcase jupyter-org--src-block-cache
	  ((and `(,x . ,_) (guard (not (eq x 'invalid))))
	   (push 'invalid jupyter-org--src-block-cache)))
	nil))))


(provide 'init-emacs-jupyter)
;;; init-emacs-jupyter.el ends here
