(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defun new-solution ()
  (make-solution
   :points '((0 0) (0 1) (1 1) (1 0))
   :facets '((0 1 2 3))
   :targets '((0 0) (0 1) (1 1) (1 0))))

(defun coerce-solution (solution f)
  (make-solution
   :points (funcall f (? solution :points))
   :facets (funcall f (? solution :facets))
   :targets (funcall f (? solution :targets))))

(defun adjustable-solution (solution)
  (coerce-solution solution #`(make-array (length %) :adjustable t :initial-contents %)))

(defun proper-solution (solution)
  (coerce-solution solution #`(coerce % 'list)))

(defun cross-product (p1 p2)
  (- (* (px p1) (py p2))
     (* (py p1) (px p2))))

(defun clockwise? (p1 p2)
  (minusp (cross-product p1 p2)))

(defun convex-problem? (problem)
  (ensure-problem!)
  (and (= 1 (length (? problem :polygons)))
       (loop :for (p1 p2 p3) :on (enclose (? problem :polygons 0) 2)
             :while p3
             :never (clockwise? (point- p2 p1) (point- p3 p2)))))
