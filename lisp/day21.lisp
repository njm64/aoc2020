(defpackage :day21
  (:use :cl :alexandria)
  (:shadowing-import-from :arrow-macros :->> :-<>> :<>))

(in-package :day21)

(defun parse-line (line)
  (destructuring-bind (a b) (str:split " (contains " line)
    (list (str:words a)
          (str:split ", " (str:substring 0 -1 b)))))

(defun parse-input (lines)
  (mapcar #'parse-line lines))

(defun build-table (input)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (ingredients allergens) in input do
      (loop for allergen in allergens do
        (if-let (existing (gethash allergen ht))
          (setf (gethash allergen ht) (intersection existing ingredients :test #'equal))
          (setf (gethash allergen ht) ingredients))))
    (hash-table-alist ht)))

(defun next-pair (table)
  (when-let (entry (find-if (lambda (e) (= (length e) 2)) table))
    (let ((ingredient (second entry)))
      (list entry (->> table
                    (remove entry)
                    (mapcar (lambda (e) (remove ingredient e :test #'equal))))))))

(defun solve-table (table)
  (loop for (e next) = (next-pair table)
        while e
        collect e
        do (setf table next)))

(defun part1 (input)
  (let* ((pairs (solve-table (build-table input)))
         (ingredients (reduce #'append (mapcar #'first input)))
         (allergen-ingredients (mapcar #'second pairs)))
    (count-if-not (lambda (i) (find i allergen-ingredients :test #'equal))
                  ingredients)))

(defun part2 (input)
  (-<>> (solve-table (build-table input))
    (sort <> #'string< :key #'first)
    (mapcar #'second)
    (str:join #\,)))

