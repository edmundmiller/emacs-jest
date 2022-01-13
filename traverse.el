(defconst jest--brackets '(("(" ")") '("{" "}") '("[" "]") '("<" ">")))
(defconst jest--quotes '('"'" "\"" "`"))

(defun jest--get-buffer-to-point () 
  (buffer-substring 1 (+ (point) 1)))

(defun jest--check-char-bracket (character)
  ())
(defun jest--check-char-quote (character)
  ())

;; TODO remove text closed with pairs but need to remain complete string
(defun jest--remove-folded-range (text) 
  (let ((stack '()))
    (dotimes (i (length text))
      ;; (print (char-equal ?t (substring text i (+ i 1))))
      (print (char-equal ?t (c-int-to-char (aref text i))))
      ;; (print (aref text i))
      )
    text))

(defun jest--get-describe-name-at-point ()
  "Get all merged names in hierarchical describe til current position"
  (let ((text (jest--remove-folded-range (jest--get-buffer-to-point))))
    (string-match "describe\(\\(.*\\)," text)
    (when-let (describe-name (match-string 1 text))
      (substring describe-name 1 -1))
    ))

(provide 'traverse.el)
