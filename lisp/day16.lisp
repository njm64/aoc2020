(defpackage :day16
  (:use :cl :split-sequence :uiop)
  (:shadowing-import-from :arrow-macros :->> :-<>> :<>))

(in-package :day16)

(defun parse-range (r)
  (mapcar #'parse-integer (str:split "-" r)))

(defun parse-rule (s)
  (destructuring-bind (name s2) (str:split ": " s)
    (destructuring-bind (r1 r2) (str:split " or " s2)
      (list name (parse-range r1) (parse-range r2)))))

(defun parse-ticket (s)
  (mapcar #'parse-integer (str:split "," s)))

(defun parse-input (lines)
  (let* ((groups (split-sequence-if #'str:emptyp lines))
         (rules (mapcar #'parse-rule (first groups)))
         (ticket (parse-ticket (second (second groups))))
         (nearby-tickets (mapcar #'parse-ticket (rest (third groups)))))
    (list rules ticket nearby-tickets)))

(defun match-rule (rule n)
  (destructuring-bind (name (a1 a2) (b1 b2)) rule 
    (declare (ignore name))
    (or (<= a1 n a2) (<= b1 n b2))))

;; Return true if the given value is valid for at least one rule.
(defun valid-value (rules value)
  (some (lambda (r) (match-rule r value)) rules))

;; Return true if the given rule is valid for all given values.
(defun valid-rule (values rule)
  (every (lambda (n) (match-rule rule n)) values))

;; Return true if all values in the ticket are valid for at least one rule.
(defun valid-ticket (rules ticket)
  (every (lambda (n) (valid-value rules n)) ticket))

;; Return the names of all rules that are valid for all the given values.
(defun valid-rule-names (rules values)
  (mapcar #'first (remove-if-not (lambda (r) (valid-rule values r)) rules)))

;; Convert a list of values into a list of (index . value) pairs
(defun add-indices (list)
  (loop for v in list
        for i from 0
        collect (cons i v)))

;; Helper function for generating field indices.
;; The accumulator is a list of (name . index) pairs.
;; Each element e is a pair of (index, list of possible names).
;; We take the set difference between the list of possible names and
;; the list of names seen so far, assert that there is only one new
;; name, and then add this (name . index) pair to the accumulator.
(defun field-indices-reducer (acc e)
  (destructuring-bind (i . names) e
    (let ((ns (set-difference names (mapcar #'car acc) :test #'equal)))
      (assert (= (length ns) 1))
      (cons (cons (first ns) i) acc))))

;; Find the indices of all fields, returning a list of (name . index)
;; pairs. First we transpose the list of tickets into a list of values
;; for each field index. We then build a list of possible valid fields
;; for each index, and sort that by number of possible fields. The
;; assumption is that the first field will have one possible name,
;; the second one will have two possible names, etc. We then run this
;; through field-indices-reducer to find the field name for each index.
(defun field-indices (rules tickets)
  (-<>> tickets
    (apply #'mapcar 'list)
    (mapcar (lambda (values) (valid-rule-names rules values)))
    (add-indices)
    (sort <> #'< :key (lambda (e) (length (cdr e))))
    (reduce #'field-indices-reducer <> :initial-value nil)))

(defun part1 (input)
  (destructuring-bind (rules ticket tickets) input
    (declare (ignore ticket))
    (->> tickets
      (apply #'append)
      (remove-if (lambda (n) (valid-value rules n)))
      (reduce #'+))))

(defun part2 (input)
  (destructuring-bind (rules ticket tickets) input
    (->> tickets
      (remove-if-not (lambda (tk) (valid-ticket rules tk)))
      (field-indices rules)
      (remove-if-not (lambda (f) (string-prefix-p "departure" (car f))))
      (mapcar #'cdr)
      (mapcar (lambda (i) (nth i ticket)))
      (reduce #'*))))
