(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

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

(defun parse-solution (path)
  (with-open-file (*standard-input* path)
    (let ((points (read-polygon)))
      (make-solution
       :points points
       :facets (collect (read) (collect (read) (read)))
       :targets (collect (length points) (parse-point (read-line)))))))
