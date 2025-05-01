;;; init-wolfram.el --- Wolfram Mathematica config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'xah-wolfram-mode)
  (let ((package-check-signature nil))
    (package-vc-install "https://github.com/xahlee/xah-wolfram-mode.git")))


(provide 'init-wolfram)
;;; init-wolfram.el ends here
