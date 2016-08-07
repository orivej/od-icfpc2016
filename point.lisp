(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defun enclose (seq &optional (n 1))
  (:= seq (coerce seq 'list))
  (concat (last seq n) seq))

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

(defun point/ (p1 p2)
  (block nil
    (map nil (lambda (x1 x2) (unless (zerop x2) (return (/ x1 x2))))
         p1 p2)))

(defun polygon-point+ (polygon point)
  (map 'vector #`(point+ % point) polygon))

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

(defun reflect-point-wrt-point (p q)
  (point- (point* q 2) p))

(defun reflect-point-wrt-segment (p segment)
  (reflect-point-wrt-point p (project-point p segment)))

(defun reflect-points-wrt-segment (ps segment)
  (map 'vector #`(reflect-point-wrt-segment % segment) ps))

(defun cross-product (p1 p2)
  (- (* (px p1) (py p2))
     (* (py p1) (px p2))))

(abbr det cross-product)

(defun clockwise? (p1 p2)
  (minusp (cross-product p1 p2)))

(defun intersect-lines (s1 s2)
  (let* ((p1 (? s1 0))
         (p2 (? s1 1))
         (p3 (? s2 0))
         (p4 (? s2 1))
         (cp (p (det p1 p2) (det p3 p4)))
         (d1 (point- p1 p2))
         (d2 (point- p3 p4))
         (scale (/ (det d1 d2))))
    (p (* scale (det cp (p (px d1) (px d2))))
       (* scale (det cp (p (py d1) (py d2)))))))
