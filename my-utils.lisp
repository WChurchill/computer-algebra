(in-package :my-utils)

(defmacro with-gensyms ((&rest names) &body body)
  "Assigns a generated name for each symbol in names."
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  "I don't really know what this does"
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@ (loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@ (loop for g in gensyms for n in names collect ``(,,g ,,n)))
	  ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
		,@body)))))

(defun xor (one two); BROKEN!!!
  "Exclusive-or operator. Returns non-nil value if one and only one input evaluates to nil."
  (once-only (one two)
    (if one
	(if two
	    nil
	    one)
	(if two
	    two
	    nil))))

(defun alpha-p (arg); BROKEN!!!
  "Accepts a character literal and determines if it is a letter."
  (dolist (char '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
		  #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
    (if (char-equal char arg)
	(return-from alpha-p t)))
  nil)

(defun number-char-p (arg)
  "Accepts a character literal and determines if it is a number."
  (dolist (num '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
    (if (char= arg num)
	(return-from number-char-p t)))
  nil)

(defun split-string (str split-char)
  "Splits a string where split-char is found and returns 
a list of strings."
  (let ((index (position split-char str)))
    (if index
      (remove "" `( ,(subseq str 0 index)
		     ,@(split-string (subseq str (1+ index)) split-char))
	      :test #'string=)
      `(,str))))

