;;;; linalg.lisp
;;;; Linear Algebra functionality
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
