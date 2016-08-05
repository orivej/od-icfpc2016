(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defun plot-problem (problem &key (normalize t) translate (points nil) title)
  (:= title (or title (typecase problem
                        (pathname (pathname-name problem))
                        (integer (fmt "~a" problem)))))
  (ensure-problem!)
  (when normalize
    (:= problem (normalize-problem problem)))
  (when translate
    (:= problem (translate-problem problem translate)))
  (when title
    (vgplot:title title :replot nil))
  (dolist (cmd '("unset border"
                 "set xrange [-1.5:1.5]"
                 "set yrange [-1.5:1.5]"
                 "set xtics 0.5"
                 "set ytics 0.5"
                 "set size ratio -1"))
    (vgplot:format-plot t cmd))
  (apply #'vgplot:plot '(0 1 1 0 0) '(0 0 1 1 0) "k;"
         (concat
          (loop :for segment :in (? problem :segments)
                :for points = (coerce segment 'list)
                :collect (mapcar #'px points)
                :collect (mapcar #'py points)
                :collect "b;")
          (when points
            (loop :for segment :in (? problem :segments)
                  :for points = (coerce segment 'list)
                  :collect (mapcar #'px points)
                  :collect (mapcar #'py points)
                  :collect "ob;"))
          (loop :for polygon :in (? problem :polygons)
                :for points = (enclose (coerce polygon 'list))
                :collect (mapcar #'px points)
                :collect (mapcar #'py points)
                :collect "r;"))))

(defun pathname-number (path)
  (parse-integer (pathname-name path)))

(defun plot-all (&key (glob #p"problems/*.txt") (start 0) end (delay 1))
  (dolist (path (subseq (sort (directory glob) #'< :key #'pathname-number ) start end))
    (plot-problem path)
    (sleep delay)))

(defun plot-solution (solution)
  (ensure-solution!)
  (apply #'vgplot:plot
         (loop :for facet :in (? solution :facets)
               :for points = (enclose (mapcar #`(? solution :points %) facet))
               :collect (mapcar #'px points)
               :collect (mapcar #'py points)
               :collect "k;")))
