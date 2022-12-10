(defpackage :day5
  (:use :cl)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day5)

(defun parse-input (lines) lines)

(defun mid (a b) (truncate (/ (+ a b 1) 2)))
(defun parse-binary (s) (parse-integer s :radix 2))

(defun seat-row (seat)
  (->> (str:substring 0 7 seat)
    (str:replace-all "F" "0")
    (str:replace-all "B" "1")
    (parse-binary)))

(defun seat-col (seat)
  (->> (str:substring 7 10 seat)
    (str:replace-all "L" "0")
    (str:replace-all "R" "1")
    (parse-binary)))

(defun seat-id (seat)
  (+ (* (seat-row seat) 8)
     (seat-col seat) ))

(defun part1 (seats)
  (apply #'max (mapcar #'seat-id seats)))

(defun part2 (seats)
  (let ((ids (sort (mapcar #'seat-id seats) #'<)))
    (loop for s in ids and e = (first ids) then (1+ e)
          while (eql s e) finally (return e))))

