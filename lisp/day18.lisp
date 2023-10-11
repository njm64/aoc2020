(defpackage :day18
  (:use :cl))

(in-package :day18)

(defparameter *tokens* nil)
(defparameter *version* 1)

(defun peek-token () (car *tokens*))
(defun consume-token () (pop *tokens*))

(defun lex (s)
  (loop for c across s unless (eq c #\Space) collect c))

(defun expr ()
  (if (= *version* 1)
      (term-v1)
      (factor)))

(defun 2op (v f)
  (ecase (consume-token)
    (#\+ (+ v (funcall f)))
    (#\* (* v (funcall f)))))

(defun term-v1 ()
  (loop for v = (primary) then (2op v #'primary)
        while (find (peek-token) '(#\+ #\*))
        finally (return v)))

(defun factor ()
  (loop for v = (term) then (2op v #'term)
        while (eq (peek-token) #\*)
        finally (return v)))

(defun term ()
  (loop for v = (primary) then (2op v #'primary)
        while (eq (peek-token) #\+)
        finally (return v)))

(defun primary ()
  (let* ((tok (consume-token))
         (digit (digit-char-p tok)))
    (cond
      (digit digit)
      ((eq tok #\()
       (prog1 (expr)
         ;; Consume the right bracket
         (consume-token))))))

(defun eval-expr (e)
  (let ((*tokens* e))
    (expr)))

(defun parse-input (lines)
  (mapcar #'lex lines))

(defun part1 (inputs)
  (let ((*version* 1))
    (reduce #'+ (mapcar #'eval-expr inputs))))

(defun part2 (inputs)
  (let ((*version* 2))
    (reduce #'+ (mapcar #'eval-expr inputs))))


