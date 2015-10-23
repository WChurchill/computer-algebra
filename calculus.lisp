(defun differentiate (function variable)
  "Accepts a tree and differentiates with respect to the given symbol"
  (cond
    ((or (numberp function)
	 (and (symbolp function)
	      (not (eql function variable))))
     0)
    ((eql variable function)
     1)
    ((eql '+ (first function))
     (append '(+) (map 'list #'(lambda (x) (differentiate x variable)) (rest function))))
    ((eql '* (first function))
     ())))

(defun integrate (funct var &optional start end)
  ;;Symbolic Integration
  funct
  var
  ;;Definite Evaluation
  (if (or start end)
      (print t)))
