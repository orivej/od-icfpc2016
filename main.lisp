(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defstruct point x y)
(defstruct polygon points)
;; (defstruct silhouette polygons)
(defstruct segment l r)
;; (defstruct skeleton segments)
(defstruct problem polygons segments)

(defmacro collect (cnt &body body)
  `(loop :repeat ,cnt :collect (progn ,@body)))

(defun parse-point (s)
  (let+ (((x y) (split #\, s)))
    (make-point
     :x (read-from-string x)
     :y (read-from-string y))))

(defun parse-segment (s)
  (let+ (((l r) (split #\Space s)))
    (make-segment :l (parse-point l) :r (parse-point r))))

(defun read-polygon ()
  (make-polygon :points (collect (read) (parse-point (read-line)))))

(defun parse (path)
  (with-open-file (*standard-input* path)
    (make-problem
     :polygons (collect (read) (read-polygon))
     :segments (collect (read) (parse-segment (read-line))))))
