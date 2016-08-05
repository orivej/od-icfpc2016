(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defun plot-problem (problem &key title)
  (when title
    (vgplot:title title))
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
                :collect (mapcar #'point-x points)
                :collect (mapcar #'point-y points)
                :collect "b;")
          (loop :for polygon :in (? problem :polygons)
                :for points = (list* (? polygon (1- (length polygon)))
                                     (coerce polygon 'list))
                :collect (mapcar #'point-x points)
                :collect (mapcar #'point-y points)
                :collect "r;"))))

(defun plot-path (path)
  (-> (parse path)
      (normalize-problem)
      (plot-problem :title (pathname-name path))))

(defun plot-all (&key (glob #p"problems/*.txt") (start 0) end)
  (dolist (path (subseq (directory glob) start end))
    (plot-path path)
    (sleep 5)))
