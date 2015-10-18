;;;; latex-converter.lisp
;; Converts from latex source to expression trees and vice versa

(defvar *filestream* )

(defun write-to-file)

(defun tree->latex (tree)
  (cond
   ((numberp tree)
    (format nil tree))))
