;;;; A function library with mathematical things and other things
(in-package :algebra)
(load "lisp/Math/unit-tester.lisp")
(declaim (optimize (debug 3)))

(defun number-decimal-p (arg)
  "Accepts a character literal and determines if it is a number or a decimal point."
  (let ((code (char-code arg)))
    (if (or (> 58 code 47)
	    (= code 46))
	arg)))

(defun alpha-char-p-test (arg)
  "Accepts a character literal and determines if it is a letter."
  (let ((code (char-code arg)))
    (if (or (> 91 code 64 )
	    (> 123 code 96))
	arg)))

(defun binary-operator-p (arg)
  "Accepts a character literal and determines if it is a binary operator (+, -, /, or *)."
  (cond ((char= #\+ arg) t) 
	((char= #\- arg) t)
	((char= #\/ arg) t)
	((char= #\* arg) t)
	((char= #\^ arg) t)
	(t nil)))

(defun parse-num (string)
  "Converts a string into an integer or floating-point."
  #|(format t "Parsing \"~a\" into a number~%" string)|#
  (with-input-from-string (in string)
    (read in)))

(defun dot-product (vector1 vector2)
  (do ((index 0 (1+ index))
       (sum 0))
      ((= index (length vector1 )) sum)
    (incf sum (* (elt vector1 index)
		 (elt vector2 index)))))

(defun square-matrix-p (matrix)
  "Returns wheter or not a matrix is square."
  (let ((height (length matrix)))
    (do ((row 0 (1+ row)))
	((= row height) t)
      (if (not (= (length (elt matrix row))
		  height))
	  (return-from square-matrix-p)))))

(defun determinant (matrix)
  (if (and (square-matrix-p matrix)
	   (= 2 (length matrix)))
      (-
       (* (elt (elt matrix 1) 0)
	  (elt (elt matrix 2) 1))
       (* (elt (elt matrix 1) 1)
	  (elt (elt matrix 2) 0)))))

(defun cross-product (vector1 vector2)
  (assert (= 3 (length vector1) (length vector2)))
  (+
   (- (* (elt vector1 1)
	 (elt vector2 2))
      (* (elt vector1 2)
	 (elt vector2 1)))
   (* -1
      (- (* (elt vector1 0)
	    (elt vector2 2))
	 (* (elt vector1 2)
	    (elt vector2 0))))
   (- (* (elt vector1 0)
	 (elt vector2 1))
      (* (elt vector1 1)
	 (elt vector2 0)))))

(defun rads->deg (num)
  (* 180 (/ num pi)))

(defun deg->rads (num)
  (/  (* num pi) 180))

(defun end-parens (sequence first-index)
  "Given a sequence and the index of an opening parenthesis, returns the index of the matching 
closing parenthesis."
  (do ((index (1+ first-index) (1+ index))
       (paren-level 1))
      ((= paren-level 0)
       (1- index))
    (let ((char (elt sequence index)))
      (if (char= #\( char)
	  (incf paren-level))
      (if (char= #\) char)
	  (decf paren-level)))))

(defun single-num-p (expression)
  "Parses the number contained in the string if there is only one number, returns nil otherwise."
  (let ((exp-length (length expression)))
    (do ((index 0 (1+ index)))	 ;loop over the input string
	((= index exp-length)	 ;terminate at the end of the string
	 (parse-num expression)) ;parse the number and return that value
      (let ((char (elt expression index))) ;store the current char for easy reference
	(if (not (number-decimal-p char)) ;if it isn't a number or a decimal
	    (if (> index 0);and it's after the beginning
		(return) 
		(if (not (or (char= char #\+) ;just ignore signs at the beginning
			     (char= char #\-)))
		    (return))))))))

(defun paren-wrapped-p (string)
  "Returns the string without the outer parentheis if the input string is wrapped 
in matching parenthesis, returns nil otherwise."
  (if (and (char= #\( (elt string 0))
	   (= (elt string (1- (length string)))
		  (end-parens string 0)))
      (parse-exp (subseq string 1 (1- (length string))))))

(defun integrate (funct var &optional start end)
  ;;Symbolic Integration
  funct
  var
  ;;Definite Evaluation
  (if (or start end)
      (print t)))

(defun parse-exp (expression)
  "Accepts a string of mathematical operators and operands and converts it into a
 manipulatable tree."
  (assert (not (string= expression "")))
;;;Base-case (single quantity);;;
  (let ((num (single-num-p expression)))
    (if num
	(return-from parse-exp num)))
  ;;put the length of the input for easy reference
  (let ((exp-length (length expression))) 
;;;Base-case (paren-wrapped expression);;;    e.g. (1+2)
    (if (and (char= #\( (elt expression 0))
	     (= (1- exp-length)
		(end-parens expression 0)))
	(return-from parse-exp (parse-exp (subseq expression 1 (1- exp-length)))))
    (let ((main-tree nil))		;instantiate the output tree
;;;Addition and Subtraction;;;
      (do ((current-index 0 (1+ current-index)) ;track the character being analyzed
	   (token-start 0) ;track the beginning of the potential token
	   (current-op '+))
	  ((= current-index exp-length) ;terminate loop at end of input
	   (if (= token-start 0) ;if there is no addition or subtraction
	       (parse-mult expression)
	       (let ((last-token (parse-exp (subseq expression
						    token-start))))		   
		 (if (eql '- current-op)
		     (push `(* -1 ,last-token) main-tree)
		     (push last-token main-tree))
		 (push '+ main-tree))))
	(case (char expression current-index)
	  ;;if it's a "(" jump to the closing parenthesis
	  (#\(
	   (setf current-index (end-parens expression current-index)))
	  (#\+
	   (unless (= current-index 0)
	     (let ((sym (parse-exp (subseq expression
					   token-start
					   current-index))))
	       (if (eql '+ current-op)
		   (push sym main-tree)
		   (push `(* -1 ,sym) main-tree)))
	     (setf current-op '+))
	   (setf token-start (1+ current-index)))
	  (#\-
	   ;;if it's not at the beginning
	   (unless (= current-index 0)
	     (let ((sym (parse-exp (subseq expression
					   token-start
					   current-index))))
	       (if (eql '+ current-op)
		   (push sym main-tree)
		   (push `(* -1 ,sym) main-tree))))
	   (setf current-op '-
		 token-start (1+ current-index))))
       main-tree))))

(deftest test-addition ()
  (check
   (= (eval (parse-exp "-3")) -3)
   (= (eval (parse-exp "+4")) 4)
   (= (eval (parse-exp "2+2")) 4)
   (= (eval (parse-exp "1+2+3+4+5+6+7+8+9+0")) 45)
   (= (eval (parse-exp "8-5")) 3)
   (= (eval (parse-exp "7-3-3-1")) 0)
   (= (eval (parse-exp "3+3-3+8-8")) 3)
   (= (eval (parse-exp "7+1-8-3-1")) -4)
   (= (eval (parse-exp "38-6+1+1")) 34)
   (= (eval (parse-exp "8-3+4-2" )) 7)
   (= (eval (parse-exp "9+1+1-2+3+32")) 44)))

(defun parse-mult (expression)
  "Accepts a string containing no addition or subtraction and converts it into
 a manipulatable tree. Multiplicands must be separated by a '*'. "
  (let ((num (single-num-p expression)))
    (if num (return-from parse-mult num)))
  (let ((exp-length (length expression)))
    (if (and
	 (char= #\( (elt expression 0))
	 (= exp-length (end-parens expression 0)))
	(parse-exp (subseq expression 1 (1- exp-length))))
    (do ((index 0 (1+ index))
	 (token-start 0)
	 (current-op '*)
	 main-tree
	 divisors)
	((= index exp-length)
	 (if (= token-start 0)
	     (parse-powers expression)
	     (let ((last-token (parse-exp (subseq expression token-start))))
	       (if (eql current-op '/)
		   (progn
		     (push last-token divisors)
		     `(* ,@main-tree
			 (expt (* ,@divisors) -1)))
		   (progn
		     (push last-token main-tree)
		     (if divisors
			 `(* ,@main-tree
			     (expt (* ,@divisors) -1))
			 `(* ,@main-tree)))))))
      (case (char expression index)
	(#\*
	 (when (eql current-op '/)
	   (push (parse-exp (subseq expression
				    token-start
				    index))
		 main-tree)
	   (return-from parse-mult `(* ,main-tree
				       ,(parse-mult (subseq
						     expression
						     (1+ index)))
				       (expt (* ,@divisors) -1))))
	 (push (parse-exp (subseq expression
				  token-start
				  index))
	       main-tree)
	 (setf token-start (1+ index)
	       current-op '*))
	(#\/
	 (if (eql current-op '/)
	     (push (parse-exp (subseq expression token-start index))
		   divisors)
	     (push (parse-exp (subseq expression token-start index))
		   main-tree))
	 (setf current-op '/
	       token-start (1+ index)))
	(#\(
	 (let ((close-index (end-parens expression index)))
	   #|(push (parse-exp (subseq expression
	   (1+ index)
	   close-index))
	main-tree)|#
	   (setf index close-index)))))))

(deftest test-multiplication ()
  (check
   (= (eval (parse-exp "1+1*2")) 3)
   (= (eval (parse-exp "1+1*(2)")) 3)
   (= (eval (parse-exp "1+2*3-4*(5*3)")) -53)
   (= (eval (parse-exp "1*2*(3)")) 6)
   (= (eval (parse-exp "(-1)*(5/2)*(-8)")) 20)
   (= (eval (parse-exp "3/4/6")) 1/6)
   (= (eval (parse-exp "(2*3)*(4*(1/2)*6)")) 72)))

(defun parse-powers (expression)
  (do ((index 0 (1+ index))
       (exp-length (length expression)))
      ((= index exp-length)
       (parse-exp expression))
    (case (char expression index)
      (#\^
       (return-from parse-powers `(expt ,(parse-exp (subseq expression
							    0
							    index))
					,(parse-powers (subseq expression
							       (1+ index))))))
      (#\(
       (setf index (end-parens expression index))))))

(deftest test-powers ()
  ())

(deftest test-functions ()
  ())

(deftest test-all ()
  (test-addition)
  (test-multiplication)
  (test-powers)
  (test-functions))

(defparameter functions
  `(+ - / * log sqrt exp expt sin cos tan summation integrate lim))

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

(defun simplify (tree)
  (assert tree)
  (cond
    ((or (numberp tree)
	 (symbolp tree))
     (return-from simplify tree))
    ((= 1 (length tree))
     (return-from simplify (simplify (first tree)))))
  (assert (find (first tree) functions))
  (let ((funct (first tree))
	(args (rest tree))
	newtree)
    (case funct
      (+
       (simplify-addition args))
      (*
       (simplify-mult args))
      (expt
       ;;Replace (expt (exp 1) ...) with (exp ...)
       (if (equal (simplify (first args)) '(exp 1))
	     (return-from simplify `(exp ,(simplify (second args))))))
      (exp
       (return-from simplify `(exp ,(simplify (first args))))))))

(deftest test-simplify ()
  (check
   (equal (simplify '(+ x))
	  'x)
   (equal (simplify '(+ 7))
	  7)
   (= (simplify '(+ 7 3))
      10)
   (= (simplify '(* 2 3))
      6)
   (equal (simplify '(+ 1 x))
	  '(+ 1 x))))

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
	  ((symbolp val)
	   (if (gethash val symbol-hash)
	       (incf (gethash val symbol-hash))
	       (setf (gethash val symbol-hash) 1)))
	  ((numberp val)
	   (push val num-stack)))))
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
  (print "simplify-mult")
  (print args)
  (if (= (length args) 1)
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
	  ((symbolp val)
	   (if (gethash val symbol-hash)
	       (incf (gethash val symbol-hash))
	       (setf (gethash val symbol-hash) 1)))
	  ((numberp val)
	   (push val num-stack)))))
    (push '* num-stack)
    (push (eval num-stack) newtree)
    (maphash #'(lambda (key value)
		 (push `(* ,value ,key) newtree)) symbol-hash)
    (maphash #'(lambda (key value)
		 (push `(* ,value ,key) newtree)) list-hash)
    (push '* newtree)))

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

(defun to-infix (tree operator)
  (if tree
      (concatenate 'string
		   (tree->string (car tree) t)
		   (if (eql nil (cdr tree))
		       ""
		       (string operator))
		   (to-infix (cdr tree) operator))
      ""))

(defun tree->string (tree &optional parens)
  (cond
    ((numberp tree)
     (format nil "~d" tree))
    ((symbolp tree)
     (string tree))
    ((listp tree)
     (let ((funct (first tree))
	   (args (rest tree))
	   (str ""))
       (case funct
	 ((* +)
	  (setf str (to-infix args funct)))
	 (expt
	  (setf str
		(concatenate 'string
			     (tree->string (car args) t) "^"
			     (tree->string (cadr args) t))))
	 (t
	  (error "~&Unknown function: ~a" funct )))
       (if parens
	   (concatenate 'string
			"(" str ")")
	   str)))))

(defun tree->latex (tree)
  (cond
   ((numberp tree)
    (format ))))
