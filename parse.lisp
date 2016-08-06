(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defvar *problems* #p"problems/")
(defvar *solutions* #p"solutions/")

(defmacro collect (cnt &body body)
  `(loop :repeat ,cnt :collect (progn ,@body)))

(defun parse-point (s)
  (mapcar #'read-from-string (split #\, s)))

(defun parse-segment (s)
  (coerce (mapcar #'parse-point (split #\Space s)) 'vector))

(defun read-polygon ()
  (collect (read) (parse-point (read-line))))

(defun parse (path)
  (with-open-file (*standard-input* path)
    (make-problem
     :polygons (collect (read) (coerce (read-polygon) 'vector))
     :segments (collect (read) (parse-segment (read-line))))))

(defun parse-solution (path)
  (with-open-file (*standard-input* path)
    (let ((points (read-polygon)))
      (make-solution
       :points points
       :facets (collect (read) (collect (read) (read)))
       :targets (collect (length points) (parse-point (read-line)))))))

(defun ensure-problem (problem)
  (etypecase problem
    (integer
     (parse (merge-pathnames (fmt "~a.txt" problem) *problems*)))
    (pathname
     (parse problem))
    (problem
     problem)))

(defun ensure-solution (solution)
  (etypecase solution
    (integer
     (parse-solution (merge-pathnames (fmt "~a.txt" solution) *solutions*)))
    (pathname
     (parse-solution solution))
    (solution
     solution)))
