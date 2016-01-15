;;;; algebra.lisp
;;;; Basic algebraic functionality
(in-package :computer-algebra)




(defun var-p (symbol)
  (not (or (numberp symbol)
	   (find symbol functions))))

(defun has-vars-p (tree)
  "Whether or not a lisp tree has variables (as opposed to explicit values)."
  (assert (not (eql tree nil)))
  (dolist (node tree nil)
    (if (listp node)
	(if (has-vars-p node)
	    (return-from has-vars-p t))
	(if (var-p node)
	  (return-from has-vars-p t)))))

(deftest test-has-vars ()
  (check
   (eql (has-vars-p '(+ 1 2 3)) nil)
   (eql (has-vars-p '(1)) nil)
   (eql (has-vars-p '(+ 2 'x)) t)
   (eql (has-vars-p '(+ (+ 1 'x) (- 3 4))) t)
   (eql (has-vars-p '(/ (log 1 2) (expt 7 3))) nil)
   (eql (has-vars-p '(expt 12 (+ 2 4 (- 89 (/ 1 2 'x))))) t)))

(defun contains-var (var tree)
  (and
   (not (numberp tree))
   (or (and (symbolp tree)
	    (eql var tree))
       (dolist (node tree nil)
	 (if (listp node)
	     (if (contains-var var node)
		 (return-from contains-var t))
	     (if (equal var node)
		 (return-from contains-var t)))))))


(defun replace-var (tree var value)
  (assert (not (eql tree nil)))
  (dotimes (index (length tree) tree)
    (let ((node (elt tree index)))
      (if (listp node)
	  (replace-var node var value)
	  (if (equal var node)
	      (progn
		(setf (elt tree index) value)))))))

(deftest test-replace-var ()
  (check
   (equal (replace-var '(x) 'x 1)
	  '(1))
   (equal (replace-var '(x) 'q 13)
	  '(x))
   (equal (replace-var '(+ 1 2 3) 'x 72)
	  '(+ 1 2 3))
   (equal (replace-var '(+ x 1 2 3 x) 'x 24)
	  '(+ 24 1 2 3 24))
   (equal (replace-var '(1) 'z 'y)
	  '(1))
   (equal (replace-var '(z) 'z 'y)
	  '(y))
   (equal (replace-var '(+ 2 'x) 'x 60)
	  '(+ 2 60))
   (equal (replace-var '(+ (+ 1 'x) (- 3 4)) 'x -7)
	  '(+ (+ 1 -7) (- 3 4)))
   (equal (replace-var '(/ (log 1 2) (expt 7 3)) 'w 'lel)
	  '(/ (log 1 2) (expt 7 3)))
   (equal (replace-var '(/ (log 1 2) w (expt 7 3) w) 'w 'lel)
	  '(/ (log 1 2) lel (expt 7 3) w))
   (equal (replace-var '(expt 12 (+ 2 4 (- 89 (/ 1 2 'x)))) 'x '(log 10 y))
	  '(expt 12 (+ 2 4 (- 89 (/ 1 2 (log 10 'y))))))))

(defun replace-vars (tree vars values)
  (assert (not (eql tree nil)))
  (assert (= (length vars) (length values)))
  (dotimes (index (length tree) tree)
    (let ((node (elt tree index)))
      (if (listp node)
	  (replace-vars node vars values)
	  (let ((pos (position node vars)))
	    (if pos
		(setf (elt tree index)
		      (elt values pos))))))))

(deftest test-replace-vars ()
  (check
   (equal (replace-vars '(x) '(x) '(1))
	  '(1))
   (equal (replace-vars '(x) '(y) '(13))
	  '(x))
   (equal (replace-vars '(+ 1 2 3) '(x) '(72))
	  '(+ 1 2 3))
   (equal (replace-vars '(+ x 1 2 3 x) '(x) '(24))
	  '(+ 24 1 2 3 24))
   (equal (replace-vars '(1) '(x) '(y))
	  '(1))
   (equal (replace-vars '(x) '(x) '(y))
	    '(y))
   (equal (replace-vars '(+ 2 'x) '(x) '(60))
	  '(+ 2 60))
   (equal (replace-vars '(+ (+ 1 'x) (- 3 4)) '(x) '(-7))
	  '(+ (+ 1 -7) (- 3 4)))
   (equal (replace-vars '(/ (log 1 2) (expt 7 3)) '(x) '(lel))
	  '(/ (log 1 2) (expt 7 3)))
   (equal (replace-vars '(/ (log 1 2) x (expt 7 3) y) '(x) '(lel))
	  '(/ (log 1 2) lel (expt 7 3) y))
   (equal (replace-vars '(expt 12 (+ 2 4 (- 89 (/ 1 2 'x)))) '(x) '((log 10 y)))
	  '(expt 12 (+ 2 4 (- 89 (/ 1 2 (log 10 'y))))))))

(defparameter functions
  `(+ * log exp expt sin cos tan sum lim))

(defun simplify (tree)
  (assert tree)
  (cond
    ((or (numberp tree)
	 (symbolp tree))
     (return-from simplify tree))
    ((= 1 (length tree))
     (return-from simplify (simplify (first tree)))))
  (let ((funct (first tree))
	(args (rest tree)))
    (assert (find (first tree) functions))
    (case funct
      (+
       (simplify-addition args))
      (*
       (simplify-mult args))
      (exp
       `(exp ,(simplify (first args))))
      (expt
       (apply #'simplify-expt args))
      (log
       (apply #'simplify-log args))
      (sum
       (apply #'simplify-summation args)))))


(defun simplify-log (arg)
  (setf arg (simplify arg)
	base (simplify base))
  `(log ,arg
	,base))

(deftest test-simplify ()
  (check
    (= (simplify '(+ 7))
       7)
    (= (simplify '(+ 7 3))
       10)
    (= (simplify '(* 2 3))
       6)
    (equal (simplify '(+ x))
	   'x)
    (equal (simplify '(+ 1 x))
	   '(+ 1 x))))

(defun simplify-expt (base power)
  ;;Replace (expt (exp 1) ...) with (exp ...)
  (if (equal (simplify base) '(exp 1))
      (return-from simplify-expt
	`(exp ,(simplify (second args)))))
  ;; x^1
  ;; x^0
  ;; 1^x
  ;; 0^x
  `(expt ,(simplify (second args))))

(defun simplify-addition (args)
  (if (= (length args) 1)
      (return-from simplify-addition (simplify (first args))))
  (let ((symbol-hash (make-hash-table :size (length args)))
	(list-hash (make-hash-table :size (length args)))
	num-stack
	newtree
	(only-nums t))
    (dolist (node args)
      (let ((val (simplify node)))
	(cond
	  ((listp val)
	   (if (eql '+ (first val))
	       (return-from simplify-addition
		 (simplify `(+ ,(splice-funct '+ args))))
	       (let ((s-val (simplify val)))
		 (setf only-nums nil)
		 (if (gethash s-val list-hash)
		     (incf (gethash val list-hash))
		     (setf (gethash val list-hash) 1)))))
	  ((integerp val)
	   (push val num-stack))
	  ((or (symbolp val)
	       (numberp val))
	   (if (gethash val symbol-hash)
	       (incf (gethash val symbol-hash))
	       (setf (gethash val symbol-hash) 1))))))
    (push '+ num-stack)
    (let ((sum (eval num-stack)))
      (if only-nums
	  (return-from simplify-addition sum)
	  (push sum newtree)))
    (maphash #'(lambda (key value)
		 (push `(* ,value ,key) newtree)) symbol-hash)
    (maphash #'(lambda (key value)
		 (push `(* ,value ,key) newtree)) list-hash)
    (push '+ newtree)))

(defun splice-funct (funct args)
  (let (newlist)
    (dolist (node args)
      (if (and (listp node)
	       (eql funct (first node)))
	  (setf newlist (append newlist (rest node)))
	  (setf newlist (append newlist (list node)))))
    newlist))

(deftest test-splice ()
  (check
   (equal (splice-funct '+ '(1 2 (+ 3 4) (+ 5 6)))
	  '(1 2 3 4 5 6))
   (equal (splice-funct '* '(1 2 (* 3 4) (* 5 6)))
	  '(1 2 3 4 5 6))
   (equal (splice-funct '+ '(1 2 (* 3 4) (* 5 6)))
	  '(1 2 (* 3 4) (* 5 6)))
   (equal (splice-funct '+ '(1 2 (+ 3 4) (* 5 6)))
	  '(1 2 3 4 (* 5 6)))
   (equal (splice-funct '* '(1 2 (+ 3 4) (* 5 6)))
	  '(1 2 (+ 3 4) 5 6))))

(defun simplify-mult (args)
  ;(print "simplify-mult")
  ;(print args)
  (if (= (length args) 1) ; if there's only one argument, return it
      (return-from simplify-mult (simplify (first args))))
  (let ((symbol-hash (make-hash-table :size (length args)))
	(list-hash (make-hash-table :size (length args)))
	num-stack
	newtree)
    (dolist (node args)
      (let ((val (simplify node)))
	(cond
	  ((listp val)
	   (if (eql '* (first val))
	       (return-from simplify-mult (simplify (splice-funct '+ args)))
	       (let ((s-val (simplify val)))
		 (if (gethash s-val list-hash)
		     (incf (gethash val list-hash))
		     (setf (gethash val list-hash) 1)))))
	  ((integerp val)
	   (push val num-stack))
	  (t
	   (assert (or (numberp val)
		       (symbolp val)))
	   (if (gethash val symbol-hash)
	       (incf (gethash val symbol-hash))
	       (setf (gethash val symbol-hash) 1))))))
    (when num-stack
      (push '* num-stack)
      (push (eval num-stack) newtree))
    (maphash #'(lambda (key value)
		 (if (= value 1)
		     (push key newtree)
		     (push `(expt ,key ,value) newtree)))
	     symbol-hash)
    (maphash #'(lambda (key value)
		 (if (= value 1)
		     (push key newtree)
		     (push `(expt ,value ,key) newtree)))
	     list-hash)
    (if (= 1 (length newtree))
	(first newtree)
	(push '* newtree))))

(defun equivalent-p (tree &rest trees)
  (assert (not (or (eql nil tree)
		   (eql nil trees))))
  (let* ((tree2 (first trees))
	 (args (rest tree))
	 (args2 (rest tree2))
	 (funct (first tree))
	 (funct2 (first tree2))
	 equivalent)
    (if (listp tree)
	(progn
	  (assert (and (find (first tree) functions)
		       (find (first tree2) functions)))
	  (case funct
	    ((+ *)
	     (let ()
	       (if (and (eql funct funct2)
			(same-quantities args args2))
		   (setf equivalent t)))))))
    (if equivalent
	(let ((other-trees (rest trees)))
	  (if other-trees
	      (equivalent-p tree other-trees)
	      t)))))

(defun print-hash (key value)
  (format t "~%~a :: ~a" key value))

(defun tables-equal (table1 table2)
  (maphash #'(lambda (key value)
	       (let ((otherval (gethash key table2)))
		 (if (not (equal value otherval))
		     (return-from tables-equal))))
	   table1)
  t)

(defun same-quantities (list1 list2)
  (let ((len1 (length list1))
	(len2 (length list2)))
    (if (not (= len1 len2))
	nil
	(let ((hash (make-hash-table :size len1))
	      (hash2 (make-hash-table :size len1))
	      (list-hash (make-hash-table :size len1 :test 'equal))
	      (list-hash2 (make-hash-table :size len1 :test 'equal)))
	  (dolist (node list1)
	    (let (table)
	      (if (listp node)
		  (setf table list-hash)
		  (setf table hash))
	      (if (eql nil (gethash node table))
		  (setf (gethash node table) 1)
		  (incf (gethash node table)))))
	  (dolist (node list2)
	    (let (table)
	      (if (listp node)
		  (setf table list-hash2)
		  (setf table hash2))
	      (if (eql nil (gethash node table))
		  (setf (gethash node table) 1)
		  (incf (gethash node table)))))
	  (and (tables-equal hash hash2)
	       (tables-equal list-hash list-hash2))))))

(deftest test-same-quantities ()
  (check
   (same-quantities '(x) '(x))
   (same-quantities '(1 2 3 4) '(1 2 3 4))
   (same-quantities '(1 2 3 (5 4 3)) '(1 2 3 (5 4 3)))
   (same-quantities '(+ - 4 x) '(x - + 4))
   (not (same-quantities '(lol lel kek lmao) '(lel kek lmao lel lol)))
   (same-quantities '(5 6 (3 4 5) (3 4 5) (3 4 5) (1 2))
		    '(5 6 (3 4 5) (3 4 5) (3 4 5) (1 2)))
   (not (same-quantities '(5 6 (3 4 5) (4 5) (3 4 5) (1 2))
			 '(5 6 (3 4 5) (3 4 5) (3 4 5) (1 2))))))

(defun count-atoms (tree)
  "Counts the atoms in a list and its sublists to give an idea of the complexity 
of an expression."
  (assert (listp tree))
  (let ((total 0))
    (dolist (node tree)
      (if (listp node)
	  (incf total (count-atoms node))
	  (incf total)))
    total))

(defun perfect-square-p (n)
  (do* ((i 1 (1+ i))
	(j 1 (* i i)))
       (nil
	nil)
    (cond
      ((> j n)
       (return-from perfect-square-p nil))
      ((= j n)
       (return-from perfect-square-p t)))))

(defun solve-for (variable equation)
  "Given a symbol as a variable and a pair of expression trees as an equation, 
isolate the specified variable and simplify the other side of the equation."
  ;;TODO add support for more than 2 sides of an equation
  ;;TODO ability to return multiple values
  ;;TODO imaginary number support

;;; ITERATION 1
;;; Only one count of x. No nested functions. Only addition in input.
;;; ITERATION 2
;;; Multiple x allowed.
;;; ITERATION 3
;;; Only addition and multiplication in input. No exponents in input.
;;; 
  (assert (equal '= (first equation))) ;equation must start with "=" symbol
  (assert (= (length (rest equation)) 2)) ;only 2 sides to an equation (for now)
  (let ((left-side (first (rest equation)))
        (right-side (second (rest equation))))
    (assert (or (contains-var variable left-side)
                (contains-var variable right-side)))
    ;;On the right side, put all expressions containing x on the left side
    (change-side right-side left-side variable
                 #'contains-var)
      
    ;;At this point, 'variable' is guaranteed to be somewhere in 'left-side'
    ;;On the left side, put all expressions NOT containing x on the right side
    (change-side left-side right-side variable
                 #'(lambda (var node) (not (contains-var var node))))
    
    right-side
    #|(do ()
    ((and (not (contains-var variable right-side))
    (equal variable left-side))
    right-side)
      
    ())|#))

(defmacro change-side (initial destination variable test-fun)
  (with-gensyms (ls transfer-stack)
    `(let (,transfer-stack)
       (do ((,ls ,initial))
           ((eql ,ls nil)
            nil)
         ;(print (car ,ls))
         (if (funcall ,test-fun ,variable (car (cdr ,ls)))
             (progn (push (car (cdr ,ls)) ,transfer-stack)
                    (setf
                     (cdr ,ls) (cdr (cdr ,ls))))
             (setf ,ls (cdr ,ls))))
       ;(format t "~&Moving ~a to the right side~%" ,transfer-stack)
       (cond
         ((eql nil ,transfer-stack) nil)
         ((= (length ,transfer-stack) 1)
          (setf ,destination (append ,destination `((* -1 ,@,transfer-stack)))))
         (t
          (setf ,destination (append ,destination `((* -1 (+ ,@,transfer-stack))))))))))

(defun test-change-side ()
  (let ((left-side '(+ 1 x y))
        (right-side '(+ 2 x)))
    (format t "left-side: ~a~%" left-side)
    (format t "right-side: ~a~%" right-side)
    (change-side right-side left-side 'x #'contains-var)
    (format t "left-side: ~a~%" left-side)
    (format t "right-side: ~a~%" right-side)))

(deftest test-solve-for ()
  (check
    (handler-case ;make sure an error is thrown
	(progn
	  (solve-for 'x '(= 1 2))
	  nil)
      (error (e) t))
    (= (solve-for 'x '(= x 4)) 4)
    (= (solve-for 'x '(= 4 x)) 4)
    (= (solve-for 'x '(= (+ x 2) 4)))
    (= (solve-for 'x '(= (+ x -2) 6)))
    (= (solve-for 'x '(= (+ 2 (* -1 x)) -2)))
    (= (solve-for 'x '(=
    		       (+ 4 (+ 2 (* -1 x)))
    		       (+ ))))))

(defun expand-polynomial (polynomial exponent)
  (assert (eql '+ (first polynomial)))
  (let ((result nil))
    (dotimes (i exponent)
      (push polynomial result))
    (push '* result)))



(defun define-theorem (exp1 exp2)
  (cons exp1 (cons exp2 nil)))

(define-theorem
    '(* -1 (* -1 x)) 'x)

(defun simplify-summation (loopvar init-value endvar expression)
  (cond
    ((integerp expression)
     `(* (+ ,endvar (* -1 ,init-value) 1) ,expression))
    ((symbolp expression)
     (if (eql expression loopvar)
	 (progn
	   (assert (= init-value 1))
	   `(* ,endvar (+ 1 ,endvar) (expt 2 -1)))
	 `(* (+ ,endvar (* -1 ,init-value) 1) ,expression)))
    ((listp expression)
     (assert (= init-value 1))
     (let ((funct (first expression))
	   (args (rest expression)))
       (case funct
	 ((+)
	  (let (result-tree)
	   (dolist (node args)
	     (push (simplify-summation loopvar init-value endvar node) result-tree))
	   (push '+ result-tree)))
	 ((*)
	  (let (constant-list loopvar-list)
	    (dolist (node args)
	      (if (contains-var loopvar node)
		  (push node loopvar-list)
		  (push node constant-list)))
	    (append (list '*) constant-list)))
	 ((sum)
	  (apply #'simplify-summation args))
	 (t
	  (error "Invalid function ~S" funct)))))
    (t
     (error "Invalid summation expression ~s." expression))))

