(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defun new-solution ()
  (make-solution
   :points '((0 0) (1 0) (1 1) (0 1))
   :facets '((0 1 2 3))
   :targets '((0 0) (1 0) (1 1) (0 1))))

(defun coerce-solution! (solution check convert)
  (dolist (key '(:points :facets :targets))
    (let ((value (? solution key)))
      (unless (funcall check value)
        (:= (slot-value solution (intern (string key)))
            (funcall convert value))))))

(defun adjustable-solution! (solution)
  (coerce-solution! solution
                    #`(and (arrayp %) (adjustable-array-p %))
                    #`(make-array (length %)
                                  :adjustable t :fill-pointer t :initial-contents %)))

(defun proper-solution! (solution)
  (coerce-solution! solution #'listp #`(coerce % 'list)))

(defun convex-problem? (problem)
  (ensure-problem!)
  (and (= 1 (length (? problem :polygons)))
       (loop :for (p1 p2 p3) :on (enclose (? problem :polygons 0) 2)
             :while p3
             :never (clockwise? (point- p2 p1) (point- p3 p2)))))

(defun solution-fold-left (solution segment)
  (adjustable-solution! solution)
  (let+ (((&slots-r/o points facets targets) solution)
         ((p1 p2) segment)
         (fwd (point- p2 p1))
         ;; sides: -1 left, 0 on, 1 right
         (sides (map 'vector #`(signum (cross-product (point- % p1) fwd)) targets))
         (progress? nil))
    (iter
      (:for facet :in-sequence facets :with-index ifacet)
      (when (and (some #`(= -1 (? sides %)) facet)
                 (some #`(= 1 (? sides %)) facet))
        (:for left = nil)
        (:for right = nil)
        (flet ((push-aside (i sign)
                 (case sign
                   (-1 (push i left))
                   (0 (push i left) (push i right))
                   (1 (push i right)))))
          (iter
            (:for (ip1 ip2) :on (enclose facet))
            (:while ip2)
            (:for s1 = (? sides ip1))
            (:for s2 = (? sides ip2))
            (assert (not (= s1 s2 0)))
            (push-aside ip1 s1)
            (when (= (* s1 s2) -1)
              (let* ((t1 (? targets ip1))
                     (t2 (? targets ip2))
                     (new-target (intersect-lines segment `(,t1 ,t2)))
                     (scale (point/ (point- new-target t1) (point- t2 t1)))
                     (p1 (? points ip1))
                     (p2 (? points ip2))
                     (new-point (scale-point-wrt-point p2 p1 scale))
                     (ip (length points)))
                (vector-push-extend new-target targets)
                (vector-push-extend new-point points)
                (push-aside ip 0)))))
        (:= (? facets ifacet) (nreverse left))
        (vector-push-extend (nreverse right) facets)))
    (iter
      (:for side :in-vector sides :with-index k)
      (when (= side 1)
        (:= progress? t)
        (:= #1=(? targets k) (reflect-point-wrt-segment #1# segment))))
    progress?))

(defun most-distant-pair (points)
  (iter
    (:for p1 :in points)
    (:for p2 = (iter
                 (:for p2 :in points)
                 (:find p2 :max (point-distance2 p1 p2))))
    (:find (list p1 p2) :max (point-distance2 p1 p2))))

(defun unit-square-bound (points)
  (let* ((center (point* (apply #'point+ points)
                         (/ (length points)))))
    (list (point+ center '(-1/2 -1/2))
          (point+ center '(1/2 -1/2))
          (point+ center '(1/2 1/2))
          (point+ center '(-1/2 1/2)))))

(defun points-within-polygon? (points polygon)
  (iter
    (:for (p1 p2) :on (enclose polygon))
    (:while p2)
    (:for d = (point- p2 p1))
    (:always
      (iter
        (:for p :in points)
        (:never (clockwise? d (point- p p1)))))))

(defun solve-problem (problem)
  (ensure-problem!)
  (when (convex-problem? problem)
    (let* ((solution (new-solution))
           (points (coerce (? problem :polygons 0) 'list))
           (bound (unit-square-bound points))
           (progress? t))
      (when (points-within-polygon? points bound)
        (:= (solution-targets solution) bound)
        (iter
          (:while progress?)
          (:= progress? nil)
          (iter
            (:for (p1 p2) :on (enclose points))
            (:while p2)
            (when (solution-fold-left solution (list p1 p2))
              (:= progress? t))))
        solution))))

(defun solve-all ()
  (dolist (n (sort (set-difference (problem-numbers) (solution-numbers)) #'<))
    (when-let (solution (solve-problem n))
      (save-solution solution n)
      (format *trace-output* "~a " n)
      (force-output *trace-output*))))
