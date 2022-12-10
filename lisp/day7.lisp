(defpackage :day7
  (:use :cl :alexandria))

(in-package :day7)

(defun parse-contents-item (s)
  (let ((tokens (str:words s)))
    (list (parse-integer (first tokens))
          (str:join " " (subseq tokens 1 3)))))

(defun parse-contents (s)
  (when (string/= s "no other bags.")
    (let ((items (str:split ", " (str:substring 0 -1 s))))
      (mapcar #'parse-contents-item items))))

(defun parse-rule (s)
  (destructuring-bind (name contents) (str:split " bags contain " s)
    (cons name (parse-contents contents))))

(defun parse-input (lines)
  (alist-hash-table (mapcar #'parse-rule lines) :test #'equal))

(defun find-bag (h outer inner)
  (or (equal outer inner)
      (find-if (lambda (b) (find-bag h (second b) inner))
               (gethash outer h)))) 

(defun count-items (h name)
  (reduce #'+ (mapcar (lambda (b)
                        (destructuring-bind (sub-count sub-name) b
                          (* sub-count (1+ (count-items h sub-name)))))
                      (gethash name h))))

(defun part1 (h)
  (1- (count-if (lambda (b) (find-bag h b "shiny gold"))
                (hash-table-keys h))))

(defun part2 (h) (count-items h "shiny gold"))

