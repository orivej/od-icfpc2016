(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defmacro ensure-problem! ()
  `(:= problem (ensure-problem problem)))

(defstruct problem polygons segments points)

(defun translate-problem (problem point)
  (ensure-problem!)
  (make-problem
   :polygons (mapcar #`(polygon-point+ % point) (? problem :polygons))
   :segments (mapcar #`(polygon-point+ % point) (? problem :segments))
   :points (mapcar #`(point+ % point) (? problem :points))))

(defun normalize-problem (problem)
  (ensure-problem!)
  (translate-problem problem (point- (? problem :polygons 0 0))))

(defun all-problem-points (problem)
  (ensure-problem!)
  (or (? problem :points)
      (-> (apply #'concat (? problem :segments))
          (delete-duplicates :test #'equal)
          (sort #'point<))))

(defun print-points (points)
  (format t "~:{~a,~a~%~}" points))
