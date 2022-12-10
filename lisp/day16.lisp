(defpackage :day16
  (:use :cl :split-sequence)
  (:export :run))

(in-package :day16)

;; So input criteria of:
;; (name, range1, range2)

(defun parse-rule (s)
  (destructuring-bind (name s2) (str:split ": " s)
    (destructuring-bind (r1 r2) (str:split " or " s2)
      (list name (str:split "-" r1) (str:split "-" r2)))))

(defun parse-ticket (s)
  (mapcar #'parse-integer (str:split "," s)))

(defun parse-input (lines)
  (let* ((groups (split-sequence-if #'str:emptyp lines))
         (rules (mapcar #'parse-rule (first groups)))
         (ticket (parse-ticket (second (second groups))))
         (nearby-tickets (mapcar #'parse-ticket (rest (third groups)))))
    (list rules ticket nearby-tickets)))

(defun part1 (lst) 0)

(defun part2 (lst) 0)

;; Maybe for each ticket field, we get a list of possible
;; input fields.
