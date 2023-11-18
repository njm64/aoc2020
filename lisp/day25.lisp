(defpackage :day25
  (:use :cl))

(in-package :day25)

(defun parse-input (lines)
  (mapcar #'parse-integer lines))

(defun transform (subject-number loop-size)
  (loop for n = 1 then (mod (* n subject-number) 20201227)
        repeat loop-size
        finally (return n)))

(defun find-loop-size (public-key)
  (loop for i = 0 then (1+ i)
        for n = 1 then (mod (* n 7) 20201227)
        until (= n public-key)
        finally (return i)))

(defun part1 (input)
  (destructuring-bind (card-public door-public) input
    (let ((card-loop-size (find-loop-size card-public)))
      (transform door-public card-loop-size))))

