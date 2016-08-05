(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defstruct problem polygons segments)

(defmacro collect (cnt &body body)
  `(loop :repeat ,cnt :collect (progn ,@body)))

(defun px (p) (first p))
(defun py (p) (second p))
(defun point< (p1 p2)
  (if (= (py p1) (py p2))
      (< (px p1) (px p2))
      (< (py p1) (py p2))))

(defun point- (p1 p2)
  (mapcar #'- p1 p2))
