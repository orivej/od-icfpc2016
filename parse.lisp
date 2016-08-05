(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

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
