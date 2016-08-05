(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defun polygon-point- (polygon point)
  (map 'vector #`(point- % point) polygon))

(defun normalize-problem (problem)
  (let ((origin (? problem :polygons 0 0)))
    (make-problem
     :polygons (->> (? problem :polygons)
                    (mapcar #`(polygon-point- % origin)))
     :segments (->> (? problem :segments)
                    (mapcar #`(polygon-point- % origin))))))

(defun problem-points (problem)
  (-> (apply #'concat (? problem :segments))
      (delete-duplicates :test #'equal)
      (sort #'point<)))

(defun print-points (points)
  (format t "~:{~a,~a~%~}" points))
