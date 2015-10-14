;;;; trigonometry.lisp

(defun rads->deg (num)
  (* 180 (/ num pi)))

(defun deg->rads (num)
  (/  (* num pi) 180))
