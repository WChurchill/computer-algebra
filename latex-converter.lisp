;;;; latex-converter.lisp
;; Converts from latex source to expression trees and vice versa
(in-package :computer-algebra)

(defvar *filestream* nil)

;(defun write-to-file)

(defun tree->latex (tree)
  (cond
    ((or (symbolp tree)
	 (numberp tree))
     (format nil "~a" tree))
    ((eql '* (first tree))
     (format nil "~{~s~^\\times~}"
	     (map 'list #'tree->latex (rest tree))))
    ((eql '+ (first tree))
     (format nil "~{~s~^+~}"
	     (map 'list #'tree->latex (rest tree))))
    ((eql 'expt (first tree))
     (format nil "~s^{~s}"
	     (tree->latex (second tree))
	     (tree->latex (third tree))))
    ((eql 'exp (first tree))
     (format nil "e^{~s}" (tree->latex (second tree))))))


