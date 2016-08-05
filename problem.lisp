(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defmacro ensure-problem! ()
  `(:= problem (ensure-problem problem)))

(defstruct problem polygons segments)

(defun translate-problem (problem point)
  (ensure-problem!)
  (make-problem
   :polygons (->> (? problem :polygons)
                  (mapcar #`(polygon-point+ % point)))
   :segments (->> (? problem :segments)
                  (mapcar #`(polygon-point+ % point)))))

(defun normalize-problem (problem)
  (ensure-problem!)
  (translate-problem problem (point- (? problem :polygons 0 0))))

(defun problem-points (problem)
  (ensure-problem!)
  (-> (apply #'concat (? problem :segments))
      (delete-duplicates :test #'equal)
      (sort #'point<)))

(defun print-points (points)
  (format t "~:{~a,~a~%~}" points))
