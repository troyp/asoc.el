;;;  -*- lexical-binding: t -*-

(require 'asoc)
(require 'ert)

(cl-macrolet ((should-equal
               (expr keyword result)
               (if (eq keyword :result)
                   `(should (equal ,expr ,result))
                 (error "expected :result"))))

  (ert-deftest test-asoc-docstring-examples-asoc-do ()
    "Docstring examples for asoc functions and macros."
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (three . 9) (4 . 16) (five . 25))))
       (asoc-do ((key value) a sum)
         (when (symbolp key)
           (setf sum (+ (or sum 0) value)))))
     :result 34)
    ;; with RESULT
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))
           (s ""))
       (asoc-do ((k v) (reverse a) s)
         (setq s (cons (format "%S\t%S" k v) s))))
     :result '("1	1" "2	4" "3	9" "4	16" "5	25")))
  (ert-deftest test-asoc-docstring-examples-asoc-fold ()
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))
           (s ""))
       (asoc-fold a ""
                  (lambda (k v acc)
                    (concat acc (format "%S\t%S\n" k v)))))
     :result "1\t1\n2\t4\n3\t9\n4\t16\n5\t25\n"))

  (ert-deftest test-asoc-unit-tests-asoc-put ()
    ;; test with replace=nil
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))))
       (asoc-put 3 10 a)
       )
     :result '((3 . 10) (1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))
    ;; test with replace=non-nil
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))))
       (asoc-put 3 10 a :replace)
       )
     :result '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25)))
    ;; test with replace=non-nil, multiple deletions
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (3 . 1) (5 . 25))))
       (asoc-put 3 10 a :replace)
       )
     :result '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25)))
    ;; test with replace=non-nil, no deletions
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (4 . 16) (5 . 25))))
       (asoc-put 3 10 a :replace)
       )
     :result '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25)))
    ;; test with replace=non-nil, deletion at head of list
    (should-equal
     (let ((a '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25))))
       (asoc-put 3 10 a :replace)
       )
     :result '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25)))
    )

  )

(defun asoc---test-all ()
  (interactive)
  (ert-run-tests-batch "^test-asoc" ))
