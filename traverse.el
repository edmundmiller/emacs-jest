(defconst jest--brackets '((?( ?)) (?{ ?}) (?[ ?]) (?< ?>)))
(defconst jest--quotes '(?\' ?\" ?\`))

(defun jest--get-buffer-to-point () 
  (buffer-substring 1 (+ (point) 1)))

(defun jest--check-template (character substitutes cb)
  (let ((ele substitutes) (result nil))
    (while ele
      (when (char-equal character (funcall cb ele))
        (setq ele '())
        (setq result t))
      (setq ele (cdr ele)))
    result))


(defun jest--check-open-bracket (character)
  (jest--check-template character jest--brackets
                    (lambda (ch) (car (car ch)))))

(defun jest--check-close-bracket (character)
  (jest--check-template character jest--brackets
                    (lambda (ch) (car (cdr (car ch))))))

(defun jest--check-char-quote (character)
  (jest--check-template character jest--quotes
                    (lambda (ch) (car ch))))

;; TODO remove text closed with pairs but need to remain complete string
(defun jest--remove-folded-range (text) 
  (let ((stack '())
        (edited text))
    (dotimes (i (length text))
      (let ((ch (c-int-to-char (aref text i))))
        (cond
         ((jest--check-open-bracket ch)
          (push (list i ch) stack)
          (print "bracket"))
         ((jest--check-close-bracket ch)
          (push (list i ch) stack)
          (print "bracket"))
         ((jest--check-char-quote ch)
          (push (list i ch) stack)
          (print "quotes"))
         (t (print "default")))))
    (print stack)
    text))

(defun jest--get-describe-name-at-point ()
  "Get all merged names in hierarchical describe til current position"
  (let ((text (jest--remove-folded-range (jest--get-buffer-to-point))))
    (string-match "describe\(\\(.*\\)," text)
    (when-let (describe-name (match-string 1 text))
      (substring describe-name 1 -1))
    ))

(provide 'traverse.el)
