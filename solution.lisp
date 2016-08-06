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

(defun make-segment-ifacet-hash-table (facets)
  (let ((ht (make-hash-table :test 'equal))
        (facets (coerce facets 'vector)))
    (iter
      (:for facet :in-vector facets :with-index k)
      (iter
        (:for (ip1 ip2) :on (enclose facet))
        (:while ip2)
        (:= (? ht `(,ip1 ,ip2)) k)))
    ht))

(defun solution-fill-targets (solution &key debug (start 0))
  (let* ((points (coerce (? solution :points) 'vector))
         (facets (coerce (? solution :facets) 'vector))
         (targets (make-array (length points) :initial-element nil))
         (segment-ifacet (make-segment-ifacet-hash-table facets))
         (visited (make-array (length facets) :initial-element nil)))
    (labels
        ((recur (ifacet ps)
           (:= (? visited ifacet) t)
           (let ((facet (? facets ifacet)))
             (when debug (print (list 'facet ifacet
                                      'ips facet
                                      'at (mapcar #`(? ps %) facet))))
             (iter
               (:for ip :in facet)
               (:for old-target = (? targets ip))
               (:for new-target = (? ps ip))
               (if (null old-target)
                   (:= (? targets ip) new-target)
                   (assert (equal old-target new-target)
                           () "Inconsistent solution. Will not move point ~a from ~a to ~a"
                           ip old-target new-target)))
             (iter
               (:for (ip1 ip2) :on (enclose facet))
               (:while ip2)
               (:for segment-ips = (list ip2 ip1))
               (:for segment = (vector (? ps ip1) (? ps ip2)))
               (:for next-ifacet = (? segment-ifacet segment-ips))
               (when (and next-ifacet (not (? visited next-ifacet)))
                 (recur next-ifacet (reflect-points-wrt-segment ps segment)))))))
      (recur start points))
    (:= (solution-targets solution) (coerce targets 'list))))
