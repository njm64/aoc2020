(defpackage :day13
  (:use :cl)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day13)

(defun parse-buses (buses)
  (mapcar #'(lambda (s) (parse-integer s :junk-allowed t))
          (str:split "," buses)))

(defun parse-input (lines)
  (list (parse-integer (first lines))
        (parse-buses (second lines))))

(defun next-departure (time freq)
  (* freq (1+ (floor (/ (1- time) freq)))))

(defun wait-time (time freq)
  (- (next-departure time freq) time))

(defun cmp-first (a b)
  (if (< (first a) (first b)) a b))

(defun find-base (base step offset freq)
  (loop for n = base then (+ n step)
        until (zerop (mod (+ n offset) freq))
        finally (return n)))

(defun part1 (input)
  (destructuring-bind (time buses) input
    (->> buses
      (remove nil)
      (mapcar (lambda (b) (list (wait-time time b) b)))
      (reduce #'cmp-first)
      (apply #'*))))

(defun part2 (input)
  (let ((base 0) (step 1) )
    (loop for freq in (second input)
          for offset = 0 then (1+ offset)
          when freq do
            (setf base (find-base base step offset freq))
            (setf step (* step freq)))
    base))
