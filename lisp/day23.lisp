(defpackage :day23
  (:use :cl)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day23)

(defun parse-input (lines)
  (map 'list #'digit-char-p (car lines)))

(defun extend (max numbers)
  (loop for m = (1+ (apply #'max numbers)) then (1+ m)
        while (<= m max)
        collect m into ext
        finally (return (append numbers ext))))

(defun make-circular (lst)
  (setf (cdr (last lst)) lst)
  nil)

(defun rotate-circular (lst n)
  (loop for head on lst
        when (= (car head) n)
          return head))

(defun break-circular (lst)
  (loop for e = lst then (cdr e)
        until (eq (cdr e) lst)
        finally (setf (cdr e) nil)))

(defun make-lookup (lst max)
  (loop with lookup = (make-array (1+ max))
        finally (return lookup)
        for e on lst do
          (setf (aref lookup (car e)) e)))

(defun remove-next-three (lst)
  (let ((sublist (cdr lst)))
    (setf (cdr lst) (cddddr lst))
    (setf (cdddr sublist) nil)
    sublist))

(defun insert-sub-list (lst sub-list)
  (setf (cdr (last sub-list)) (cdr lst))
  (setf (cdr lst) sub-list))

(defun pick-destination (current excluding max)
  (flet ((next (n) (if (> n 1) (1- n) max)))
    (loop for n = (next current) then (next n)
          while (find n excluding)
          finally (return n))))

(defun run (iterations cups)
  (let* ((current (copy-list cups))
         (max (reduce #'max cups))
         (lookup (make-lookup current max)))
    (make-circular current)
    (loop repeat iterations do
      (let* ((next3 (remove-next-three current))
             (dst (pick-destination (car current) next3 max)))
        (insert-sub-list (aref lookup dst) next3))
      (setf current (cdr current)))
    (setf current (rotate-circular current 1))
    (break-circular current)
    current))

(defun part1 (input)
  (reduce (lambda (acc n) (+ (* acc 10) n))
          (run 9 input)))

(defun part2 (input)
  (let ((cups (run 10000000 (extend 1000000 input))))
    (* (elt cups 1)
       (elt cups 2))))
