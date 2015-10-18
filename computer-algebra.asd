;;;; computer-algebra.asdf

(asdf:defsystem #:computer-algebra
  :description "A library designed to solve algebra, calculus and linear algebra problems."
  :author "Winston Carlile wjc140030@utdallas.edu"
  :license "Apache 2.0 License"
  :serial t
  :depends-on ("utilities")
  :components ((:file "package")
	       (:file "computer-algebra")))
