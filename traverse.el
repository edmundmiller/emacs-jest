(defconst jest--pairs '('("'" "'") '("\"" "\"") '("`" "`")
                        '("(" ")") '("{" "}") '("[" "]") '("<" ">"))
  "open and closed pairs")
(defun jest--get-buffer-to-point () 
  (buffer-substring 1 (+ (point) 1)))

(defun jest--remove-folded-range (text) 
  text)

(defun jest--get-describe-name-at-point ()
  (let ((text (jest--remove-folded-range (jest--get-buffer-to-point))))
    (string-match "describe\(\\(.*\\)," text)
    (when-let (describe-name (match-string 1 text))
      (substring describe-name 1 -1))
    ))

(provide 'traverse.el)
