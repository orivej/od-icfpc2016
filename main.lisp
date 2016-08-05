(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defstruct problem polygons segments)

(defmacro collect (cnt &body body)
  `(loop :repeat ,cnt :collect (progn ,@body)))

(defun parse-point (s)
  (mapcar #'read-from-string (split #\, s)))

(defun parse-segment (s)
  (coerce (mapcar #'parse-point (split #\Space s)) 'vector))

(defun read-polygon ()
  (coerce (collect (read) (parse-point (read-line))) 'vector))

(defun parse (path)
  (with-open-file (*standard-input* path)
    (make-problem
     :polygons (collect (read) (read-polygon))
     :segments (collect (read) (parse-segment (read-line))))))

(defun point-x (p) (first p))
(defun point-y (p) (second p))

(defun point- (p1 p2)
  (mapcar #'- p1 p2))

(defun polygon-point- (polygon point)
  (map 'vector #`(point- % point) polygon))

(defun normalize-problem (problem)
  (let ((origin (? problem :polygons 0 0)))
    (make-problem
     :polygons (->> (? problem :polygons)
                    (mapcar #`(polygon-point- % origin)))
     :segments (->> (? problem :segments)
                    (mapcar #`(polygon-point- % origin))))))

(defun plot-problem (problem &key title)
  (when title
    (vgplot:title title))
  (vgplot:axis (list -1.5 1.5 -1.5 1.5))
  (vgplot:format-plot t "set xtics 0.5")
  (vgplot:format-plot t "set ytics 0.5")
  (vgplot:format-plot t "set size ratio -1")
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

(defun plot-all (&key (glob #p"problems/*.txt") (start 0) end)
  (dolist (path (subseq (directory glob) start end))
    (-> (parse path)
        (normalize-problem)
        (plot-problem :title (pathname-name path)))
    (sleep 5)))
