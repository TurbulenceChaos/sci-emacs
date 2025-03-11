;;; Create a comment divider of exactly 80 characters based on TEXT.
;;; The format is: ;; --- <TEXT> --- ;;.

(defun comment-divider (text)
  "Create a comment divider of exactly 80 characters based on TEXT.
The format is: ;; --- <TEXT> --- ;;."
  (interactive "sEnter text for divider: ")
  (let* ((total-length 80) ; desired length
         (comment-prefix ";; ") ; Comment prefix (start)
         (comment-suffix " ;;") ; Comment suffix (end, includes leading space)
         (prefix-length (length comment-prefix))
         (suffix-length (length comment-suffix))
         (text-with-spaces (concat " " text " ")) ; Add spaces around the input text
         (text-length (length text-with-spaces))
         (dash-length (- total-length prefix-length suffix-length text-length)) ; Remaining space for dashes
         (half-dash-length (/ dash-length 2)) ; Divide remaining space equally
         (divider (concat (make-string half-dash-length ?-)
                          text-with-spaces
                          (make-string (- dash-length half-dash-length) ?-))))
    (insert comment-prefix divider comment-suffix)))

(provide 'comment-divider)
