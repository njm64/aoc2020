(defpackage :day9
  (:use :cl))

(in-package :day9)

(defparameter *preamble-size* 25)

(defun parse-input (lines)
  (let ((nums (mapcar #'parse-integer lines)))
    (make-array (length nums) :initial-contents nums)))

(defun check-valid (nums index)
  (let ((key (aref nums index)))
    (loop for i from (- index *preamble-size*) below index do
      (loop for j from (1+ i) below index do
        (when (= key (+ (aref nums i) (aref nums j)) )
          (return-from check-valid t))))))

(defun find-first-invalid (nums)
  (loop for i from *preamble-size* below (length nums) do
    (when (not (check-valid nums i))
      (return (aref nums i)))))

(defun find-range (nums n)
  (loop for i from 0 below (length nums) do
    (let ((sum 0))
      (loop for j from i below (length nums) do
        (incf sum (aref nums j))
        (when (= sum n)
          (return-from find-range (list i j)))))))

(defun calc-score (nums range)
  (destructuring-bind (a b) range
    (+ (loop for i from a to b minimizing (aref nums i))
       (loop for i from a to b maximizing (aref nums i)))))

(defun part1 (nums)
  (find-first-invalid nums))

(defun part2 (nums)
  (let* ((n (find-first-invalid nums))
         (r (find-range nums n)))
    (calc-score nums r)))

