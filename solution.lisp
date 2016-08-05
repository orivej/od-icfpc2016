(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defmacro ensure-solution! ()
  `(:= solution (ensure-solution solution)))

(defstruct solution points facets targets)

(defun solution->problem (solution)
  (ensure-solution!)
  (make-problem
   :polygons nil
   :segments (-> (loop :for facet :in (? solution :facets)
                       :for points = (enclose (mapcar #`(? solution :targets %) facet))
                       :append (mapcar #'vector points (rest points)))
                 (delete-duplicates :test #'equalp))))
