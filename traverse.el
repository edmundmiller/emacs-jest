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

(defun jest--check-bracket-pair (open close)
  (catch 'result
    (dolist (x jest--brackets)
      (when (and (char-equal open (car x))
                 (char-equal close (car (cdr x))))
        (throw 'result t)))))

(defun jest--check-char-quote (character)
  (jest--check-template character jest--quotes
                    (lambda (ch) (car ch))))

(defun jest--is-in-quotes (stack)
  (if (car stack)
      (jest--check-char-quote (car (car stack)))
    nil))

(defun jest--merge-collapsed-range (list-to-del new-one)
  "compare the last input of list with new-one
  If collapse remove last one and push
  If not, just push"
  (if (or (not (car list-to-del))
          (< (car (cdr (car list-to-del))) (car new-one)))
      (push new-one list-to-del)
    (push new-one (cdr list-to-del))))

(defun jest--remove-ranges (text list-to-del)
  (let ((edited text))
    (dolist (cursor list-to-del)
      (setq edited (concat (substring edited 0 (+ 1 (car cursor)))
                        (substring edited (car (cdr cursor))))))
    edited))

;; todo remove text closed with pairs but need to remain complete string
;; 1 : When meet Quotes.
;;      If it's same quotes, pop from stack
;            if not, Inside quotes ignore. If not inside quote, add to stack.
;; 2 : When meet bracket
;;      If it's inside quotes, do nothing.
;; 2-1:   If not inside quotes and opening braket
;;        add to stack and record index.
;; 2-2: ... and closing braket
;;        remove from stack and add list do delete with opening braket index.
;;              If the last inserted in list is collapsed with current one, remove last one and add.
(defun jest--remove-folded-range (text) 
  (let ((stack '())
        (list-to-del '())
        (edited text))
    (dotimes (i (length text))
      (let ((ch (c-int-to-char (aref text i))))
        (cond
         ((jest--check-char-quote ch)
          (if (jest--is-in-quotes stack)
              (when (char-equal (car (car stack)) ch)
                (setq stack (cdr stack)))
            (push (list ch i) stack)))
         ((jest--check-open-bracket ch)
          (when (not (jest--is-in-quotes stack))
            (push (list ch i) stack)))
         ((and (jest--check-close-bracket ch)
               (jest--check-bracket-pair (car (car stack)) ch))
          (when (not (jest--is-in-quotes stack))
            (setq list-to-del
                  (jest--merge-collapsed-range list-to-del
                                               (list (car (cdr (car stack))) i)))
            (setq stack (cdr stack)))))))
    (jest--remove-ranges text list-to-del)))

(defun jest--get-describe-name-at-point ()
  "Get all merged names in hierarchical describe til current position"
  (let ((text (jest--remove-folded-range (jest--get-buffer-to-point))))
    (string-match "describe\(\\(.*\\)," text)
    (when-let (describe-name (match-string 1 text))
      (substring describe-name 1 -1))
    ))

(provide 'traverse.el)
