(defpackage :day3
  (:use :cl))

(in-package :day3)

(defun parse-input (lines)
  (let ((h (length lines))
        (w (length (first lines))))
    (make-array (list h w) :initial-contents lines)))

(defun count-trees (m dx dy)
  (destructuring-bind (h w) (array-dimensions m)
    (loop for x = dx then (+ x dx)
          for y = dy then (+ y dy)
          while (< y h)
          count (equal (aref m y (mod x w)) #\#))))

(defun part1 (m)
  (count-trees m 3 1))

(defun part2 (m)
  (* (count-trees m 1 1)
     (count-trees m 3 1)
     (count-trees m 5 1)
     (count-trees m 7 1)
     (count-trees m 1 2)))
