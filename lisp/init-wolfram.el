;;; init-wolfram.el --- Wolfram Mathematica config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'sci-wolfram)
  (package-vc-install
   '(sci-wolfram :url "https://github.com/TurbulenceChaos/sci-wolfram.git"
		 :branch "main")))

;; (add-to-list 'load-path "~/.emacs.d/elpa/sci-wolfram/")
;; (require 'sci-wolfram)
;; (require 'sci-wolfram-jupyter)


(provide 'init-wolfram)
;;; init-wolfram.el ends here
