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
