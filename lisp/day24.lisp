(defpackage :day24
  (:use :cl :split-sequence :alexandria))

(in-package :day24)

(defparameter *dir-map* '(:e #C(1 0) :w #C(-1 0)
                          :nw #C(-0.5 -1) :ne #C(0.5 -1)
                          :sw #C(-0.5 1) :se #C(0.5 1)))

(defparameter *dir-offsets*
  (loop for lst on *dir-map* by #'cddr collect (second lst)))

(defun next-dir (s)
  (find-if (lambda (d) (uiop:string-prefix-p d s))
           '("e" "w" "nw" "sw" "ne" "se")))

(defun parse-line (line)
  (loop for s = line then (subseq s (length d))
        for d = (next-dir s) while d
        collect (intern (string-upcase d) :keyword)))

(defun parse-input (lines)
  (mapcar #'parse-line lines))

(defun dirs-to-offset (dirs)
  (reduce #'+ (mapcar (lambda (d) (getf *dir-map* d)) dirs)))

(defun get-black-tiles (input)
  (let ((ht (make-hash-table)))
    (loop for dirs in input do
      (incf (gethash (dirs-to-offset dirs) ht 0)))
    (loop for tile being the hash-keys in ht using (hash-value n)
          when (oddp n) collect tile)))

(defun make-tile-table (tiles)
  (loop with ht = (make-hash-table)
        finally (return ht)
        for tile in tiles do
          (setf (gethash tile ht) t)))

(defun make-neighbour-table (tiles)
  (let ((ht (make-hash-table)))
    (loop for tile in tiles do
      (loop for delta in *dir-offsets* do
        (incf (gethash (+ tile delta) ht 0))))
    ht))

(defun run-day (black-tiles)
  (let ((neighbour-counts (make-neighbour-table black-tiles))
        (black-table (make-tile-table black-tiles)))
    (append
     (remove-if (lambda (tile)
                  (let ((n (gethash tile neighbour-counts 0)))
                    (or (= n 0) (> n 2))))
                black-tiles)
     (loop for tile being the hash-keys in neighbour-counts
           when (and (= (gethash tile neighbour-counts) 2)
                     (not (gethash tile black-table)))
             collect tile))))

(defun part1 (input)
  (length (get-black-tiles input)))

(defun part2 (input)
  (let ((black-tiles (get-black-tiles input)))
    (loop for tiles = black-tiles then (run-day tiles)
          repeat 100
          finally (return (length tiles)))))


