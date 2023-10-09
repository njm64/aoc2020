(defpackage :day17
  (:use :cl :alexandria) 
  (:export :run))

(in-package :day17)

(defun active-indices (line)
  (loop for c across line
        for i from 0
        when (eq c #\#)
          collect i))

(defun parse-input (lines)
  (loop for line in lines
        for y from 0
        append (mapcar (lambda (x) (list x y 0))
                       (active-indices line))))

(defun add-dimension (cube)
  (cons 0 cube))

(defun neighbour-offsets (d)
  (remove (make-list d :initial-element 0)
          (apply #'map-product 'list
                 (make-list d :initial-element '(-1 0 1)))
          :test #'equal))

(defun iterate (cubes offsets)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for c in cubes do
      (loop for offset in offsets do
        (incf (gethash (mapcar #'+ c offset) ht 0))))
    (loop for k in cubes
          when (eq (gethash k ht) 2) do
            (setf (gethash k ht) 3))
    (loop for k being the hash-keys in ht using (hash-value v)
          when (eq v 3) collect k)))

(defun run (cubes neighbours)
  (loop for cs = cubes then (iterate cs neighbours)
        repeat 6
        finally (return (length cs))))

(defun part1 (cubes)
  (run cubes (neighbour-offsets 3)))

(defun part2 (cubes)
  (run (mapcar #'add-dimension cubes) (neighbour-offsets 4)))


