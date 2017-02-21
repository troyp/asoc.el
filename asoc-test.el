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
     (with-temp-buffer
       (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))))
         (asoc-do ((k v) a)
           (insert (format "%S\t%S\n" k v))))
       (buffer-string))
     :result "1	1\n2	4\n3	9\n4	16\n5	25\n")
    ;; with RESULT
    (should-equal
     (let ((a '((one . 1) (two . 4) (3 . 9) (4 . 16) (five . 25) (6 . 36))))
       (let ((sum 0))
         (asoc-do ((key value) a sum)
           (when (symbolp key)
             (setf sum (+ sum value))))))
     :result 30)
    (ert-deftest test-asoc-docstring-examples-asoc-fold ()
      (should-equal
       (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))
             (s ""))
         (asoc-fold a ""
                    (lambda (k v acc)
                      (concat acc (format "%S\t%S\n" k v)))))
       :result "1\t1\n2\t4\n3\t9\n4\t16\n5\t25\n"))
    )

  (ert-deftest test-asoc-unit-tests-asoc-do ()
    "Docstring examples for asoc functions and macros."
    ;; error if the variable RESULT is not defined
    (should-error
     (let ((a '((one . 1) (two . 4) (3 . 9) (4 . 16) (five . 25) (6 . 36))))
       (let (sum)
         (makunbound 'sum)
         (asoc-do ((key value) a sum)
           (when (symbolp key)
             (setf sum (+ sum value)))))))
    )

  (ert-deftest test-asoc-unit-tests-asoc--compare ()
    "Unit tests for asoc--compare"
    (should-equal
     (let (table)
       (dolist (fn
                '(#'equalp #'equal #'eql #'eq)
                table)
         (let* ( result
                 (fnresult (dolist (xy
                                    '((3 . 3)           ;; 1. int
                                      (3 . 3.0)         ;; 2. int vs float
                                      ((1 2) . (1 2))   ;; 3. lists
                                      ("a" . "a")       ;; 4. strings
                                      ("a" . "A")       ;; 5. strings, diff case
                                      (x . x))          ;; 6. symbols
                                    result)
                             (setq result (cons (asoc--compare (car xy) (cdr xy))
                                                result)))))
           (setq table (cons fnresult table)))))
     :result '((t nil t t nil t)    ;; equalp
               (t nil t t nil t)    ;; equal
               (t nil t t nil t)    ;; eql
               (t nil t t nil t)))  ;; eq
    )

  (ert-deftest test-asoc-unit-tests-asoc-put ()
      ;; test with replace=nil
      (should-equal
       (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))))
         (asoc-put 3 10 a))
       :result '((3 . 10) (1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))
      ;; test with replace=non-nil
      (should-equal
       (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))))
         (asoc-put 3 10 a :replace))
       :result '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25)))
      ;; test with replace=non-nil, multiple deletions
      (should-equal
       (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (3 . 1) (5 . 25))))
         (asoc-put 3 10 a :replace))
       :result '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25)))
      ;; test with replace=non-nil, no deletions
      (should-equal
       (let ((a '((1 . 1) (2 . 4) (4 . 16) (5 . 25))))
         (asoc-put 3 10 a :replace))
       :result '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25)))
      ;; test with replace=non-nil, deletion at head of list
      (should-equal
       (let ((a '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25))))
         (asoc-put 3 10 a :replace))
       :result '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25)))
      )

  )

(defun asoc---test-all ()
  (interactive)
  (ert-run-tests-batch "^test-asoc" ))
