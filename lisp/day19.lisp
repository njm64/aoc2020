(defpackage :day19
  (:use :cl :split-sequence :alexandria))

(in-package :day19)

(defun parse-integers (s)
  (mapcar #'parse-integer (str:words s)))

(defun parse-rule (s)
  (destructuring-bind (lhs rhs) (str:split ": " s)
    (cons (parse-integer lhs)
          (if (eq (elt rhs 0) #\")
              (elt rhs 1)
              (mapcar #'parse-integers (str:split " | " rhs))))))

(defun parse-input (lines)
  (let ((sections (split-sequence-if #'str:emptyp lines)))
    (list (mapcar #'parse-rule (first sections))
          (second sections))))

(defun match-rule (msg rules rt)
  (if (and msg rules)
      ;; If we we've got a message and a rule, look up the rule
      (let ((r (gethash (car rules) rt)))
        (if (characterp r)
            ;; If it's a character literal rule, make sure the first
            ;; character matches, then recurse to check the rest.
            ;; If it doesn't match, return nil to fail the match.
            (if (char= r (car msg))
                (match-rule (cdr msg) (cdr rules) rt)
                nil)
            ;; Otherwise it's a list of sequences. For each sequence,
            ;; replace the head of the rules list with the sequence,
            ;; and try to match. We only need one to match.
            (some (lambda (s)
                    (match-rule msg (append s (cdr rules)) rt))
                  r)))
      ;; If both the message and rules are nil, it's a match.
      (and (not msg) (not rules))))

(defun count-matches (rt msgs)
  (count-if (lambda (m) (match-rule (coerce m 'list) '(0) rt)) msgs))

(defun part1 (input)
  (destructuring-bind (rules msgs) input
    (let ((rt (alist-hash-table rules :test #'equal)))
      (count-matches rt msgs))))

(defun part2 (input)
  (destructuring-bind (rules msgs) input
    (let ((rt (alist-hash-table rules :test #'equal)))
      (setf (gethash 8 rt) '((42) (42 8)))
      (setf (gethash 11 rt) '((42 31) (42 11 31)))
      (count-matches rt msgs))))
