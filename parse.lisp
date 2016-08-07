(in-package #:od-icfpc2016)
(named-readtables:in-readtable rutils-readtable)

(defvar *problems* #p"problems/")
(defvar *solutions* #p"solutions/")

(defun problem-path (n)
  (merge-pathnames (fmt "~a.txt" n) *problems*))

(defun solution-path (n)
  (merge-pathnames (fmt "~a.txt" n) *solutions*))

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
     (parse (problem-path problem)))
    (pathname
     (parse problem))
    (problem
     problem)))

(defun ensure-solution (solution)
  (etypecase solution
    (integer
     (parse-solution (solution-path solution)))
    (pathname
     (parse-solution solution))
    (solution
     (proper-solution! solution)
     solution)))

(defun save-solution (solution path)
  (ensure-solution!)
  (when (integerp path)
    (:= path (solution-path path)))
  (with-open-file (*standard-output* path :direction :output :if-exists :supersede)
    (format t "~a~%" (length (? solution :points)))
    (print-points (? solution :points))
    (format t "~a~%" (length (? solution :facets)))
    (dolist (facet (? solution :facets))
      (format t "~a~{ ~a~}~%" (length facet) facet))
    (print-points (? solution :targets))))

(defun pathname-number (path)
  (parse-integer (pathname-name path)))

(defun file-numbers (glob)
  (sort (mapcar #'pathname-number (directory glob)) #'<))

(defun problem-numbers ()
  (file-numbers (merge-pathnames "*.txt" *problems*)))

(defun solution-numbers ()
  (file-numbers (merge-pathnames "*.txt" *solutions*)))
