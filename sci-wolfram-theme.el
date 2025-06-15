;;; sci-wolfram-theme.el --- custom theme for faces  -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

(deftheme sci-wolfram)

(custom-theme-set-faces
 'sci-wolfram
 '(default ((t (:background "honeydew"))))
 '(font-lock-constant-face ((t (:foreground "black" :bold t))))
 '(font-lock-variable-name-face ((t (:foreground "red" :bold t)))))


(provide-theme 'sci-wolfram)
;;; sci-wolfram-theme.el ends here
