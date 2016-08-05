(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defstruct problem polygons segments)

(defstruct solution points facets targets)

(defun translate-problem (problem point)
  (make-problem
   :polygons (->> (? problem :polygons)
                  (mapcar #`(polygon-point+ % point)))
   :segments (->> (? problem :segments)
                  (mapcar #`(polygon-point+ % point)))))

(defun normalize-problem (problem)
  (translate-problem problem (point- (? problem :polygons 0 0))))

(defun problem-points (problem)
  (-> (apply #'concat (? problem :segments))
      (delete-duplicates :test #'equal)
      (sort #'point<)))

(defun print-points (points)
  (format t "~:{~a,~a~%~}" points))
