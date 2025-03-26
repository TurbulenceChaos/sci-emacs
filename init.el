;;; init.el --- Load the full configuration
;; SCI-emacs
;; Author: Peng Peng
;; Email:  211110103110@stu.just.edu.cn
;; GitHub: https://github.com/TurbulenceChaos/SCI-emacs
;; Package-Requires: (emacs "30.1")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defun recursive-add-to-load-path (directory)
  "Recursively add Emacs Lisp (.el) files in DIRECTORY to load-path.
Skips directories containing 'test' in their path."
  (when (file-directory-p directory)
    (let ((default-directory (file-name-as-directory directory)))
      (dolist (file (directory-files-recursively 
                     default-directory 
                     "\\.el$" 
                     nil 
                     (lambda (dir)
                       (not (string-match-p (regexp-opt '("test" "Test")) dir)))))
        (let* ((file-dir (file-name-directory file))
               (normalized-dir (file-name-as-directory 
                                (expand-file-name file-dir))))
          (unless (member normalized-dir load-path)
            (add-to-list 'load-path normalized-dir)))))))

(recursive-add-to-load-path (expand-file-name "site-lisp" user-emacs-directory))

(require 'init-basic)
(require 'init-completion)
(require 'init-magit)
(require 'init-undo)
(require 'init-keycast)
(require 'init-emacs-jupyter)
(require 'init-wolfram)
(require 'init-org)


(provide 'init)
;;; init.el ends here
