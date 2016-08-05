(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defstruct problem polygons segments)

(defstruct solution points facets targets)

(defmacro collect (cnt &body body)
  `(loop :repeat ,cnt :collect (progn ,@body)))

(defun p (x y) (list x y))
(defun px (p) (first p))
(defun py (p) (second p))
(defun point< (p1 p2)
  (if (= (py p1) (py p2))
      (< (px p1) (px p2))
      (< (py p1) (py p2))))

(defun point+ (&rest ps)
  (apply #'mapcar #'+ ps))

(defun point- (p &rest ps)
  (apply #'mapcar #'- p ps))

(defun point* (p scale)
  (mapcar #`(* % scale) p))

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

(defun dot-product (p1 p2)
  (apply #'+ (mapcar #'* p1 p2)))

(defun project-point (p segment)
  (let* ((p0 (elt segment 0))
         (p1 (elt segment 1))
         (d (point- p1 p0))
         (scale (/ (dot-product (point- p p0) d)
                   (dot-product d d))))
    (point+ p0 (point* d scale))))

(defun reflect-point (p segment)
  (let ((pr (project-point p segment)))
    (point- (point* pr 2) p)))
