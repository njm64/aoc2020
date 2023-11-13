(defpackage :day22
  (:use :cl :split-sequence)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day22)

(defun parse-player (lines)
  (mapcar #'parse-integer (rest lines)))

(defun parse-input (lines)
  (->> (split-sequence-if #'str:emptyp lines)
    (mapcar #'parse-player)))

(defun pick-v1 (state)
  (destructuring-bind (a b) state
    (> (car a) (car b))))

(defun subdeck (deck)
  (subseq (cdr deck) 0 (car deck)))

(defun pick-v2 (state)
  (destructuring-bind (a b) state
    (if (and (>= (length (cdr a)) (car a))
             (>= (length (cdr b)) (car b)))
        (consp (first (run-game #'pick-v2 (list (subdeck a) (subdeck b)))))
        (> (car a) (car b)))))

(defun run-turn (pick input)
  (destructuring-bind (a b) input
    (if (funcall pick input)
        (list (append (cdr a) (list (car a) (car b))) (cdr b))
        (list (cdr a) (append (cdr b) (list (car b) (car a)))))))

(defun run-game (pick input)
  (loop with ht = (make-hash-table :test #'equal)
        for state = input then (if (gethash state ht)
                                   '((1) nil)
                                   (run-turn pick state))
        and prev = state
        do (setf (gethash prev ht) t)
        while (every #'consp state)
        finally (return state)))

(defun calc-score (deck)
  (loop for factor = 1 then (1+ factor)
        for card in (reverse deck)
        sum (* card factor)))

(defun part1 (input)
  (->> input (run-game #'pick-v1) (find-if #'consp) calc-score))

(defun part2 (input)
  (->> input (run-game #'pick-v2) (find-if #'consp) calc-score))

