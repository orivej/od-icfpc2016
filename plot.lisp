(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defun setup-plot ()
  (dolist (cmd '("unset border"
                 "set xrange [-1.5:1.5]"
                 "set yrange [-1.5:1.5]"
                 "set xtics 0.5"
                 "set ytics 0.5"
                 "set size ratio -1"))
    (vgplot:format-plot t cmd)))

(defun plot-problem (problem &key (normalize t) translate (points nil) title)
  (:= title (or title (typecase problem
                        (pathname (pathname-name problem))
                        (integer (fmt "~a" problem)))))
  (ensure-problem!)
  (when normalize
    (:= problem (normalize-problem problem)))
  (when translate
    (:= problem (translate-problem problem translate)))
  (setup-plot)
  (when title
    (vgplot:title title :replot nil))
  (let* ((all-points (all-problem-points problem)))
    (when points
      (loop :for point :in all-points
            :for k :from 0
            :for s = (fmt "set label ~a \"~a\" at ~a, ~a"
                          (1+ k) k (float (px point)) (float (py point)))
            :do (vgplot:format-plot t s)))
    (apply #'vgplot:plot '(0 1 1 0 0) '(0 0 1 1 0) "k;"
           (concat
            (loop :for segment :in (? problem :segments)
                  :for points = (coerce segment 'list)
                  :collect (mapcar #'px points)
                  :collect (mapcar #'py points)
                  :collect "b;")
            (loop :for polygon :in (? problem :polygons)
                  :for points = (enclose (coerce polygon 'list))
                  :collect (mapcar #'px points)
                  :collect (mapcar #'py points)
                  :collect "r;")))))

(defun pathname-number (path)
  (parse-integer (pathname-name path)))

(defun plot-all (&key (glob #p"problems/*.txt") (start 0) end (delay 1))
  (dolist (path (subseq (sort (directory glob) #'< :key #'pathname-number ) start end))
    (plot-problem path)
    (sleep delay)))

(defun plot-solution (solution)
  (ensure-solution!)
  (setup-plot)
  (apply #'vgplot:plot
         (loop :for facet :in (? solution :facets)
               :for points = (enclose (mapcar #`(? solution :points %) facet))
               :collect (mapcar #'px points)
               :collect (mapcar #'py points)
               :collect "k;")))

(defun plot-solution-points (solution)
  (ensure-solution!)
  (setup-plot)
  (let ((points (? solution :points)))
    (loop :for point :in points
          :for k :from 0
          :for s = (fmt "set label ~a \"~a\" at ~a, ~a"
                        (1+ k) k (float (px point)) (float (py point)))
          :do (vgplot:format-plot t s))
    (vgplot:plot
     (mapcar #'px points)
     (mapcar #'py points)
     "+b;")))
