(defpackage :day10
  (:use :cl :org.tfeb.hax.memoize))

(in-package :day10)

(defun parse-input (lines) 
  (mapcar #'parse-integer lines))

(defun sort-and-extend (jolts)
  (let* ((max (apply #'max jolts))
         (ex (list* 0 (+ 3 max) jolts)))
    (sort (copy-seq ex) #'<)))

(defun get-diffs (jolts)
  (loop for j in jolts
        and prev = nil then j
        when prev
          collect (- j prev)))

(def-memoized-function count-permutations (jolts)
  (if (<= (length jolts) 1)
      1
      (let ((f (first jolts)))
        (loop for j on (rest jolts)
              while (<= (- (first j) f) 3)
              sum (count-permutations j)))))

(defun part1 (jolts)
  (let ((d (get-diffs (sort-and-extend jolts))))
    (* (count 1 d) (count 3 d))))

(defun part2 (jolts)
  (count-permutations (sort-and-extend jolts)))

