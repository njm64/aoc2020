(defpackage :day20
  (:use :cl :split-sequence :alexandria)
  (:shadowing-import-from :arrow-macros :->>))

(in-package :day20)

(defun parse-id (s)
  (parse-integer (str:replace-using (list "Tile " "" ":" "") s)))

(defun parse-img (lines)
  (let ((w (length (first lines)))
        (h (length lines)))
    (make-array (list h w) :initial-contents lines)))

(defparameter *seamonster*
  (parse-img '("                  # "
               "#    ##    ##    ###"
               " #  #  #  #  #  #   ")))

(defun parse-tile (lines)
  (list (parse-id (first lines))
        (parse-img (rest lines))))

(defun parse-input (lines)
  (mapcar #'parse-tile (split-sequence-if #'str:emptyp lines)))

(defun get-edge (img f)
  (let ((size (array-dimension img 0)))
    (loop for i from 0 below size
          for (x . y) = (funcall f i)
          collect (aref img (mod y size) (mod x size)) into cs
          finally (return (concatenate 'string cs)))))

(defun count-hashes (img)
  (destructuring-bind (h w) (array-dimensions img)
    (loop for y below h sum
          (loop for x below w
                count (char= (aref img y x) #\#)))))

(defun n-edge (img) (get-edge img (lambda (i) (cons i 0))))
(defun s-edge (img) (get-edge img (lambda (i) (cons i -1))))
(defun w-edge (img) (get-edge img (lambda (i) (cons 0 i))))
(defun e-edge (img) (get-edge img (lambda (i) (cons -1 i))))

(defun transform (img f)
  (let* ((size (array-dimension img 0))
         (copy (make-array (list size size))))
    (loop for x from 0 below size do
      (loop for y from 0 below size
            for (sx . sy) = (funcall f x y size) do
              (setf (aref copy y x) (aref img sy sx))))
    copy))

(defun rol (img) (transform img (lambda (x y s) (cons (- s y 1) x))))
(defun flip (img) (transform img (lambda (x y s) (cons x (- s y 1)))))

(defun get-neighbours (pos)
  (destructuring-bind (x . y) pos
    (list (cons (1+ x) y)
          (cons (1- x) y)
          (cons x (1+ y))
          (cons x (1- y)))))

(defun fit (ht img pos)
  (destructuring-bind (x . y) pos
    (let ((w (gethash (cons (1- x) y) ht))
          (e (gethash (cons (1+ x) y) ht))
          (n (gethash (cons x (1- y)) ht))
          (s (gethash (cons x (1+ y)) ht)))
      (and
       (or (not w) (equal (e-edge (second w)) (w-edge img)))
       (or (not e) (equal (w-edge (second e)) (e-edge img)))
       (or (not n) (equal (s-edge (second n)) (n-edge img)))
       (or (not s) (equal (n-edge (second s)) (s-edge img)))))))

(defun permutations (img)
  (loop for f in (list #'identity #'rol #'rol #'rol #'flip #'rol #'rol #'rol)
        for i = img then (funcall f i)
        collect i))

(defun find-tile (ht tiles pos)
  (loop for (id img) in tiles do
    (loop for p in (permutations img)
          when (fit ht p pos) do
            (return-from find-tile (list id p)))))

(defun remove-tile (list id)
  (remove-if (lambda (tile) (= (first tile) id)) list))

(defun solve (tiles)
  (loop with ht = (make-hash-table :test #'equal)
        with neighbours = '((0 . 0))
        finally (return ht)
        while tiles
        for p = (pop neighbours) do
          (assert p)
          (when-let (tile (find-tile ht tiles p))
            (setf (gethash p ht) tile)
            (setf tiles (remove-tile tiles (first tile)))
            (loop for n in (get-neighbours p) do
              (pushnew n neighbours)))))

(defun bounds (ht)
  (loop for (x . y) being the hash-keys in ht
        minimize x into x1
        maximize x into x2
        minimize y into y1
        maximize y into y2
        finally (return (list x1 y1 x2 y2))))

(defun corners (ht)
  (destructuring-bind (x1 y1 x2 y2) (bounds ht)
    (list (cons x1 y1) (cons x1 y2) (cons x2 y1) (cons x2 y2))))

(defun combine-tiles (ht)
  (destructuring-bind (x1 y1 x2 y2) (bounds ht)
    (let* ((tile-size (array-dimension (second (gethash (cons x1 y1) ht)) 0))
           (inner-size (- tile-size 2))
           (height (* (- x2 x1 -1) inner-size))
           (width (* (- y2 y1 -1) inner-size))
           (data (make-array (list height width))))
      (loop for x from x1 to x2 do
        (loop for y from y1 to y2 do
          (let ((src (second (gethash (cons x y) ht))))
            (loop for tx from 0 below inner-size do
              (loop for ty from 0 below inner-size do
                (setf (aref data
                            (+ (* (- y y1) inner-size) ty)
                            (+ (* (- x x1) inner-size) tx))
                      (aref src (1+ ty) (1+ tx))))))))
      data)))

(defun match-sub-image (img sub bx by w h)
  (loop for x from 0 below w do
    (loop for y from 0 below h do
      (when (and (char= (aref sub y x) #\#)
                 (char= (aref img (+ by y) (+ bx x)) #\.))
        (return-from match-sub-image nil))))
  t)

(defun count-sub-images (img sub)
  (destructuring-bind (bh bw) (array-dimensions img)
    (destructuring-bind (sh sw) (array-dimensions sub)
      (loop for x from 0 below (- bw sw)
            sum (loop for y from 0 below (- bh sh)
                      count (match-sub-image img sub x y sw sh))))))

(defun count-monsters (img)
  (->> (permutations img)
    (mapcar (lambda (i) (count-sub-images i *seamonster*)))
    (reduce #'max)))

(defun part1 (tiles)
  (let ((ht (solve tiles)))
    (->> (corners ht)
      (mapcar (lambda (c) (first (gethash c ht))))
      (reduce #'*)) ))

(defun part2 (tiles)
  (let* ((img (combine-tiles (solve tiles)))
         (num-monsters (count-monsters img))
         (total-waves (count-hashes img))
         (monster-waves (count-hashes *seamonster*)))
    (- total-waves (* num-monsters monster-waves))))

