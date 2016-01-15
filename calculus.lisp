;;;; calculus.lisp

(in-package :computer-algebra)

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
     (let* ((args (rest function))
	    (arg1 (first args))
	    (arg2 (rest args)))
       ()))
    ((eql 'ln (first function))
     (cond ;(log number &optional base)
      ((= (length function) 3)
       (let ((arg (second function))
             (base (third function)))
         `(* ,(differentiate arg variable)
             (expt (* ,arg (log ,base) )
                   -1))))
      ((= (length function) 2)
       `(* ,(differentiate (rest function) variable)
           (expt arg -1)))
      (t
       (error "Malformed log expression: ~s" function))))
    ((eql 'expt (first function))
     (if (contains-var variable (second function))
         (if (contains-var variable (third function))
             ;; x^x
	     (print "i dunno lol")
	     ;; x^constant
             `(* ,(third function) (expt ,(second function) (+ ,(third function) -1) )))
	 ;; constant^x
	 ;;`(* )
	 (print "to be implemented")))
    ((eql 'exp (first function))
     `(* ,(differentiate (rest function) variable)
         (exp ,(rest function))))))

(defun integrate (funct var &optional start end)
  ;;Symbolic Integration
  ;funct
  ;var
  ;;Definite Evaluation
  (if (or start end)
      (print t)))
