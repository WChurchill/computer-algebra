;;;; latex-converter.lisp
;; Converts from latex source to expression trees and vice versa
(in-package :computer-algebra)

(defvar *filestream* nil)

;(defun write-to-file)

(defun tree->latex (tree &optional (stream nil))
  (assert tree)
  (cond
    ((or (symbolp tree)
	 (numberp tree))
     (format stream "~a" tree))
    ((eql '* (first tree))
     (format stream "~{~a~^\\times ~}"
	     (map 'list #'tree->latex (rest tree))))
    ((eql '+ (first tree))
     (format stream "~{~a~^+~}"
	     (map 'list #'tree->latex (rest tree))))
    ((eql '= (first tree))
     (format stream "~{~a~^&=&~}"
	     (map 'list #'tree->latex (rest tree))))
    ((eql 'exp (first tree))
     (format stream "e^{~a}" (tree->latex (second tree))))
    ((eql 'expt (first tree))
     (format stream "~a^{~a}"
	     (tree->latex (second tree))
	     (tree->latex (third tree))))
    ((eql 'log (first tree))
     (if (= (length (rest tree)) 2)
	 (format stream "\\log_{~a}{~a}" (third tree) (second tree))
	 (format stream "\\log{~a}" (second tree))))))

(defun test-latex ()
  (format t "~a~%" (tree->latex '(+ 1 2 3 x)))
  (format t "~a~%" (tree->latex '(* 1 2 3 x)))
  (format t "~a~%" (tree->latex '(* (+ 1 2) x)))
  (format t "~a~%" (tree->latex '(log x 2)))
  (format t "~a~%" (tree->latex '(expt 20 -1)))
  (format t "~a~%" (tree->latex '(exp 3))) )

(defun write-to-file (equations-list filename)
  "Writes a sequence of equations to a specified file."
  (with-open-file (out filename :direction :output
		       :if-exists :overwrite
		       :if-does-not-exist :create)
    (format out "\\documentclass{article}~%")
    (format out "\\usepackage{amsmath}~%")
    (format out "\\usepackage{amsfonts}~%")
    (format out "\\usepackage{fullpage}~%")
    (format out "~%")
    (format out "\\begin{document}~%\\pagestyle{empty}~%\\begin{equation*}~%~2t\\begin{aligned}~%")
    (format out "")
    (dolist (eqn equations-list)
      (format out "~4t")
      (tree->latex eqn out)
      (format out "\\\\~%"))
    ;;print the equations nice and pretty
    (format out "~2t\\end{aligned}~%\\end{equation*}~%\\end{document}")))
