(defpackage :day12
  (:use :cl))

(in-package :day12)

(defun parse-cmd (line)
  (list (elt line 0)
        (parse-integer (str:substring 1 nil line))))

(defun parse-input (lines) (mapcar #'parse-cmd lines))

(defun move (pos cmd)
  (destructuring-bind (x y a) pos
    (destructuring-bind (action value) cmd
      (ecase action
        (#\N (list x (+ y value) a))
        (#\S (list x (- y value) a))
        (#\W (list (- x value) y a))
        (#\E (list (+ x value) y a))
        (#\L (list x y (mod (- a value) 360)))
        (#\R (list x y (mod (+ a value) 360)))
        (#\F (ecase a
               (0 (move pos (list #\E value)))
               (90 (move pos (list #\S value)))
               (180 (move pos (list #\W value)))
               (270 (move pos (list #\N value)))))))))

(defun rotate-right (wx wy angle)
  (loop repeat (/ angle 90) do
    (rotatef wx wy)
    (setf wy (- wy)))
  (list wx wy))

(defun rotate-left (wx wy angle)
  (loop repeat (/ angle 90) do
    (rotatef wx wy)
    (setf wx (- wx)))
  (list wx wy))

(defun move2 (pos cmd)
  (destructuring-bind (x y wx wy) pos
      (destructuring-bind (action value) cmd
        (ecase action
          (#\N (list x y wx (+ wy value)))
          (#\S (list x y wx (- wy value)))
          (#\W (list x y (- wx value) wy))
          (#\E (list x y (+ wx value) wy))
          (#\L (list* x y (rotate-left wx wy value))) 
          (#\R (list* x y (rotate-right wx wy value)))
          (#\F (list (+ x (* wx value))
                     (+ y (* wy value))
                     wx wy))))))

(defun manhattan-distance (pos)
  (let ((x (first pos))
        (y (second pos)))
    (+ (abs x) (abs y))))

(defun part1 (cmds)
  (let ((pos (reduce #'move cmds :initial-value '(0 0 0))))
    (manhattan-distance pos)))

(defun part2 (cmds)
  (let ((pos (reduce #'move2 cmds :initial-value '(0 0 10 1))))
    (manhattan-distance pos)))
