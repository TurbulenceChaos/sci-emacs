;;; init-wolfram.el --- Wolfram Mathematica config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'xah-wolfram-mode)
  (package-vc-install "https://github.com/xahlee/xah-wolfram-mode.git"))

(setq save-abbrevs nil)

(provide 'init-wolfram)
;;; init-wolfram.el ends here
