(defpackage :day6
  (:use :cl :split-sequence)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day6)

(defun parse-input (lines)
  (->> lines
    (mapcar #'(lambda (s) (coerce s 'list)))
    (split-sequence-if #'not)))

(defun count-any (g) (length (reduce #'union g)))
(defun count-all (g) (length (reduce #'intersection g)))

(defun part1 (groups) (apply #'+ (mapcar #'count-any groups)))
(defun part2 (groups) (apply #'+ (mapcar #'count-all groups)))

