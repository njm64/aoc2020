(defpackage :day11
  (:use :cl :alexandria)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day11)

(defparameter *is-adjacency-occupied* nil)
(defparameter *max-occupied* 0)

(defun parse-input (lines)
  (let ((w (length (first lines)))
        (h (length lines)))
    (make-array (list h w) :initial-contents lines)))

(defun in-range (map p)
  (destructuring-bind (h w) (array-dimensions map)
    (destructuring-bind (y x) p
      (and (>= x 0) (< x w) (>= y 0) (< y h)))))

(defun is-empty (map p) (eql #\L (apply #'aref map p)))
(defun is-occupied (map p) (eql #\# (apply #'aref map p)))
(defun add-offset (p offset) (mapcar #'+ offset p))

(defun is-adjacency-occupied-v1 (map p offset)
  (let ((a (add-offset p offset)))
    (and (in-range map a)
         (is-occupied map a))))

(defun is-adjacency-occupied-v2 (map p offset)
  (loop
    (setf p (add-offset p offset))
    (cond ((not (in-range map p)) (return nil))
          ((is-empty map p)       (return nil))
          ((is-occupied map p)    (return t)))))

(defun occupied-adjacency-count (map p)
  (loop for offset in
        '((-1 -1) (-1 0) (-1 1)
          ( 0 -1)        ( 0 1)
          ( 1 -1) ( 1 0) ( 1 1))
        count (funcall *is-adjacency-occupied* map p offset)))

(defun occupied-count (map)
  (destructuring-bind (h w) (array-dimensions map)
    (loop for x from 0 below w
          sum (loop for y from 0 below h
                    count (is-occupied map (list y x))))))

(defun update-map (src)
  (let ((dst (copy-array src)))
    (destructuring-bind (h w) (array-dimensions src)
      (loop for x from 0 below w do
        (loop for y from 0 below h do
          (let* ((p (list y x))
                 (occ (occupied-adjacency-count src p)))
            (cond ((and (is-empty src p) (zerop occ))
                   (setf (aref dst y x) #\#))
                  ((and (is-occupied src p) (>= occ *max-occupied*))
                   (setf (aref dst y x) #\L))))))
      dst)))

(defun count-iterations (map)
  (loop for m = map then (update-map m) and prev = m
        until (equalp m prev)
        finally (return (occupied-count m))))

(defun part1 (map)
  (let ((*is-adjacency-occupied* #'is-adjacency-occupied-v1)
        (*max-occupied* 4))
    (count-iterations map)))

(defun part2 (map)
  (let ((*is-adjacency-occupied* #'is-adjacency-occupied-v2)
        (*max-occupied* 5))
    (count-iterations map)))

