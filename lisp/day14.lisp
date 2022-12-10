(defpackage :day14
  (:use :cl :alexandria))

(in-package :day14)

(defun add-bit (m b)
  (logior (ash m 1) b))

(defun parse-mask (s)
  (let ((ma 0) (mb 0))
    (loop for c across s do
      (ecase c
        (#\0
         (setf ma (add-bit ma 1))
         (setf mb (add-bit mb 0)))
        (#\1 (setf ma (add-bit ma 1))
         (setf mb (add-bit mb 1)))
        (#\X (setf ma (add-bit ma 0))
         (setf mb (add-bit mb 0)))))
    (list ma mb)))

(defun parse-mem (s)
  (if (and (uiop:string-prefix-p "mem[" s)
           (uiop:string-suffix-p s "]"))
      (parse-integer (str:substring 4 -1 s))
      (error "invalid op")))

(defun parse-op (s)
  (destructuring-bind (lhs rhs) (str:split " = " s)
    (if (equal lhs "mask")
        (list* :set-mask (parse-mask rhs))
        (list :set-mem (parse-mem lhs) (parse-integer rhs)))))

(defun parse-input (lines)
  (mapcar #'parse-op lines))

(defun set-addr-bit (addresses b)
  (let ((m (ash 1 b)))
    (mapcar (lambda (a) (logior a m)) addresses)))

(defun clr-addr-bit (addresses b)
  (let ((m (ash 1 b)))
    (mapcar (lambda (a) (logandc2 a m)) addresses)))

(defun get-addresses (addr ma mb)
  (loop for b from 0 to 35
        with addresses = (list addr)
        finally (return addresses) do
          (cond
            ((and (logbitp b ma) (logbitp b mb))
             ;; Mask bit is 1. Set it for all addresses
             (setf addresses (set-addr-bit addresses b)))
            ((not (logbitp b ma))
             ;; Mask bit is X. Set both 1 and 0 for all addresses
             (setf addresses (append (set-addr-bit addresses b)
                                     (clr-addr-bit addresses b)))))))

(defun part1 (program)
  (let ((mem (make-hash-table)) (ma 0) (mb 0))
    (loop for op in program do
      (ecase (first op)
        (:set-mask
         (destructuring-bind (a b) (rest op)
           (setf ma a)
           (setf mb b)))
        (:set-mem
         (destructuring-bind (addr val) (rest op)
           (setf (gethash addr mem) (logior (logandc2 val ma) mb))))))
    (apply #'+ (hash-table-values mem))))

(defun part2 (program)
  (let ((mem (make-hash-table)) (ma 0) (mb 0))
    (loop for op in program do
      (ecase (first op)
        (:set-mask
         (destructuring-bind (a b) (rest op)
           (setf ma a)
           (setf mb b)))
        (:set-mem
         (destructuring-bind (addr val) (rest op)
           (loop for a in (get-addresses addr ma mb) do
             (setf (gethash a mem) val))))))
    (apply #'+ (hash-table-values mem))))

