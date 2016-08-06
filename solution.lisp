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
                 (delete-duplicates :test #'equalp))
   :points (? solution :points)))

(defun problem->solution (problem)
  (ensure-problem!)
  (let* ((all-points (all-problem-points problem))
         (point-index (make-hash-table :test 'equal)))
    (loop :for point :in all-points
          :for k :from 0
          :do (:= (? point-index point) k))
    (make-solution
     :points all-points
     :facets (loop :for segment :in (? problem :segments)
                   :collect (list (? point-index (? segment 0))
                                  (? point-index (? segment 1))))
     :targets all-points)))

(defun solution-reflect-points (solution ipoints segment-ipoints)
  (with-slots (points) solution
    (let ((segment (map 'vector #`(? points %) segment-ipoints)))
      (dolist (ipoint ipoints)
        (:= #1=(? points ipoint)
            (reflect-point-wrt-segment #1# segment))))))

(defun solution-copy-points (solution ipoints)
  (with-slots (points targets) solution
    (let ((start (length points))
          (new-points (mapcar #`(? points %) ipoints))
          (new-targets (mapcar #`(? targets %) ipoints)))
      (:= points (concat points new-points)
          targets (concat targets new-targets))
      (range start (length points)))))
