(defpackage :day8
  (:use :cl))

(in-package :day8)

(defun parse-instruction (s)
  (destructuring-bind (opcode operand) (str:words s)
    (list (intern (string-upcase opcode) :keyword)
          (parse-integer operand))))

(defun parse-input (lines)
  (let ((is (mapcar #'parse-instruction lines)))
    (make-array (length is) :initial-contents is)))

(defun run (mem)
  (let ((pc 0)
        (acc 0)
        (vis (make-array (length mem) :initial-element nil)))
    (loop until (or (= pc (length mem)) (aref vis pc)) do
      (setf (aref vis pc) t)
      (destructuring-bind (opcode operand) (aref mem pc)
        (ecase opcode
          (:nop (incf pc))
          (:acc (incf acc operand) (incf pc))
          (:jmp (incf pc operand)))))
    (list acc pc)))

(defun toggle (mem i)
  (destructuring-bind (opcode operand) (aref mem i)
    (case opcode
      (:nop (setf (aref mem i) (list :jmp operand)))
      (:jmp (setf (aref mem i) (list :nop operand))))))

(defun run-with-toggle (mem i)
  (toggle mem i)
  (let ((ret (run mem)))
    (toggle mem i)
    ret))

(defun run-fixed (mem)
  (loop for i from 0 below (length mem) do
    (destructuring-bind (acc pc) (run-with-toggle mem i)
      (if (= pc (length mem))
          (return acc)))))

(defun part1 (mem) (first (run mem)))
(defun part2 (mem) (run-fixed mem))
