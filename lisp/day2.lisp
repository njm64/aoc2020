(defpackage :day2
  (:use :cl))

(in-package :day2)

(defun parse-record (line)
  (cl-ppcre:register-groups-bind (a b c password)
      ("(\\d+)-(\\d+) (.): (.*)" line)
    (when (and a b c)
      (list (parse-integer a)
            (parse-integer b)
            (char c 0)
            password))))

(defun parse-input (lines)
  (mapcar #'parse-record lines))

(defun check1 (r)
  (destructuring-bind (a b c password) r
      (let ((n (count c password)))
        (and (>= n a) (<= n b)))))

(defun check2 (r)
  (destructuring-bind (a b c password) r
    (let ((ta (equal (char password (1- a)) c))
          (tb (equal (char password (1- b)) c)))
      (not (equal ta tb)))))

(defun part1 (records) (count-if #'check1 records))
(defun part2 (records) (count-if #'check2 records))
