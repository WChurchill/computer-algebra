;;;; trigonometry.lisp
(in-package :computer-algebra)

(defun rads->deg (num)
  (* 180 (/ num pi)))

(defun deg->rads (num)
  (/  (* num pi) 180))

(defmacro define-identity (&rest args))

;; sin(2a) = 2sin(a)cos(a)
(define-identity
    `(sin theta)
    `(* 2 (sin (* theta (expt 2 -1))) (cos (* theta (expt 2 -1)))))

;; cos(2a) = 2cos(a)^2-1
(define-identity
    `(cos theta)
    `(+ (expt (cos (* theta (expt 2 -1))) 2)
	(- (expt (sin (* theta (expt 2 -1))) 2))))

(define-identity
    `(cos theta)
    `(+ (- 1) (* 2 (expt (cos (* theta
				 (expt 2 -1)))
			 2))))

(define-identity
    `(cos theta)
    `(+ 1 (- (* 2 (expt (sin (* theta
				(expt 2 -1)))
			2))) ))

;; tan(2a) = 
(define-identity
    `(tan theta)
    `(* (* 2 (tan (* theta
		     (expt 2 -1))))
	(expt (+ 1 (- (tan (* theta
			      (expt 2 -1)))))
	      -1)))

;; tan(a) = sin(a)/cos(a)
(define-identity
    `(tan theta)
    `(* (sin theta)
	(expt (cos theta) -1) ))

;; sin(a)^2 = (1 - cos(2a))/2
(define-identity
    `(expt (sin theta) 2)
    `(* (+ 1 (- (cos (* 2 theta)))) (expt 2 -1)))

;; cos(a)^2 = (1 + cos(2a))/2
(define-identity
    `(expt (cos theta) 2)
    `(* (+ 1 (cos (* 2 theta))) (expt 2 -1)))

;; tan(a)^2 = (1-cos(2a))/(1+cos(2a))
(define-identity
    `(expt (tan theta) 2)
    `(* (+ 1 (- (cos (* 2 theta))))
	(expt (+ 1 (cos (* 2 theta))) -1)))


