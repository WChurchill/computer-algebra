;;;; ascii-converter.lisp

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


