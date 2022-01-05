(defconst jest--brackets '("(" ")") '("{" "}") '("[" "]") '("<" ">"))
(defconst jest--strings '('("'" "'") '("\"" "\"") '("`" "`")))

(defun jest--get-buffer-to-point () 
  (buffer-substring 1 (+ (point) 1)))

;; TODO remove text closed with pairs but need to remain complete string
(defun jest--remove-folded-range (text) 
  (let ((stack '()))
    text))

(defun jest--get-describe-name-at-point ()
  "Get all merged names in hierarchical describe til current position"
  (let ((text (jest--remove-folded-range (jest--get-buffer-to-point))))
    (string-match "describe\(\\(.*\\)," text)
    (when-let (describe-name (match-string 1 text))
      (substring describe-name 1 -1))
    ))

(provide 'traverse.el)
