(defpackage :day15
  (:use :cl :alexandria))

(in-package :day15)

(defun parse-input (lines)
  (mapcar #'parse-integer (str:split "," (first lines))))

(defun say (ht i n)
  (let ((prev (gethash n ht)))
    (setf (gethash n ht) i)
    (if prev
        (- i prev)
        0)))

(defun run (numbers turns)
  ;; Run one less than the number of turns, because we're
  ;; returning the value for the next turn.
  (loop for i from 0 below (1- turns)
        and next = 0 then (say ht i (if (< i (length numbers))
                                        (nth i numbers)
                                        next))
        with ht = (make-hash-table)
        finally (return next)))

(defun part1 (numbers) (run numbers 2020))
(defun part2 (numbers) (run numbers 30000000))

