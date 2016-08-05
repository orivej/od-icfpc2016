(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defstruct problem polygons segments)

(defstruct solution points facets targets)

(defmacro collect (cnt &body body)
  `(loop :repeat ,cnt :collect (progn ,@body)))

(defun px (p) (first p))
(defun py (p) (second p))
(defun point< (p1 p2)
  (if (= (py p1) (py p2))
      (< (px p1) (px p2))
      (< (py p1) (py p2))))

(defun point+ (&rest ps)
  (apply #'mapcar #'+ ps))

(defun point- (p1 &rest ps)
  (apply #'mapcar #'- p1 ps))

(defun checked-isqrt (n)
  (let ((q (isqrt n)))
    (assert (= n (* q q)))
    q))

(defun rsqrt (r)
  (/ (checked-isqrt (numerator r))
     (checked-isqrt (denominator r))))

(defun point-distance (p1 p2)
  (rsqrt (+ (expt (- (px p1) (px p2)) 2)
            (expt (- (py p1) (py p2)) 2))))
