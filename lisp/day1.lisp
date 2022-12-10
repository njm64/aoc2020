(defpackage :day1
  (:use :cl :alexandria)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day1)

(defun parse-input (lines)
  (mapcar #'parse-integer lines))

(defun combinations (lst length)
  (let ((r nil))
    (map-combinations #'(lambda (p) (push p r)) lst :length length)
    r))

(defun part1 (lst)
  (->> (combinations lst 2)
    (find-if #'(lambda (a) (= (apply #'+ a) 2020)))
    (apply #'*)))

(defun part2 (lst)
  (->> (combinations lst 3)
    (find-if #'(lambda (a) (= (apply #'+ a) 2020)))
    (apply #'*)))
