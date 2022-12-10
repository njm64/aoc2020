(defpackage :day4
  (:use :cl :split-sequence :alexandria :cl-ppcre)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day4)

(defparameter *required-keys* '(:byr :iyr :eyr :hgt :hcl :ecl :pid))

(defun parse-field (f)
  (destructuring-bind (k v) (str:split #\: f)
    (list (intern (string-upcase k) :keyword)
          v)))

(defun parse-record (line)
  (->> line
    (str:words)
    (mapcar #'parse-field)
    (apply #'concatenate 'list)))

(defun parse-input (lines)
  (->> lines
    (split-sequence-if #'str:emptyp)
    (mapcar #'(lambda (lst) (str:join " " lst)))
    (mapcar #'parse-record)))

(defun match-regex (r s)
  (multiple-value-bind (s groups) (cl-ppcre:scan-to-strings r s)
    (when s (coerce groups 'list))))

(defun check-year (s min-year max-year)
  (when (scan-to-strings "^(\\d+)$" s)
    (<= min-year (parse-integer s) max-year)))

(defun check-hgt (s)
  (multiple-value-bind (s groups) (scan-to-strings "^(\\d+)(cm|in)$" s)
    (when s
      (let ((n (parse-integer (aref groups 0)))
            (u (aref groups 1)))
        (cond
          ((equal u "cm") (<= 150 n 193))
          ((equal u "in") (<= 59 n 76)))))))

(defun check-hcl (s) (scan-to-strings "^#([0-9|a-f]){6}" s))
(defun check-ecl (s) (scan-to-strings "^(amb|blu|brn|gry|grn|hzl|oth)$" s))
(defun check-pid (s) (scan-to-strings "^(\\d){9}$" s))

(defun check1 (r)
  (every #'(lambda (k) (getf r k)) *required-keys*))

(defun check2 (r)
  (and (check-year (getf r :byr) 1920 2002)
       (check-year (getf r :iyr) 2010 2020)
       (check-year (getf r :eyr) 2020 2030)
       (check-hgt (getf r :hgt))
       (check-hcl (getf r :hcl))
       (check-ecl (getf r :ecl))
       (check-pid (getf r :pid))))

(defun part1 (records) (count-if #'check1 records))
(defun part2 (records) (count-if #'check2 records))
