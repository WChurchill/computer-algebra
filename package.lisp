;;;; package.lisp

(defpackage #:computer-algebra
  (:use #:cl #:utilities)
  (:nicknames #:coal)
  (:export :differentiate
	   :integrate
	   :solve-for
	   :tree->latex
	   :parse-ascii))
