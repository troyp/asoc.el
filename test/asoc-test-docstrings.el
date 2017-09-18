;;;  -*- lexical-binding: t -*-

(require 'asoc)
(require 'ert)

(cl-macrolet
    ((should-equal            (expr &key result)
                              `(should (equal ,expr ,result)))
     (should-not-equal        (expr &key result)
                              `(should-not (equal ,expr ,result)))
     (should-error-with       (expr &key error)
                              `(should (equal (should-error ,expr)
                                              ,error)))
     (should-error-with-type  (expr &key error)
                              `(should (equal (car (should-error ,expr))
                                              ,error))) )

  ;; ,-----------------,
  ;; | Docstring Tests |
  ;; '-----------------'

  (ert-deftest test-asoc-docstring-examples-asoc-merge ()
    "Docstring examples for `asoc-merge'."
    (should-equal
     (asoc-merge '((a . 1) (b . 2) (a . 4))
                 '((a . 4) (c . 5) (c . 6)))
     :result '((a . 4) (c . 5) (b . 2))))

  (ert-deftest test-asoc-docstring-examples-asoc-do ()
    "Docstring examples for `asoc-do'."
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
     :result 30))

  (ert-deftest test-asoc-docstring-examples-asoc--do ()
    "Docstring examples for `asoc--do'."
    (should-equal
     (let ((a '((one . 1) (two . 4) (3 . 9) (4 . 16) (five . 25) (6 . 36))))
       (asoc--do a
         (when (symbolp key)
           (setf result (+ (or result 0) value)))))
     :result 30)
    (should-equal
     (let ((a '((one . 1) (two . 4) (3 . 9) (4 . 16) (five . 25) (6 . 36))))
       (asoc--do a
         (:initially (setf result 0))
         (when (symbolp key)
           (setf result (+ result value)))))
     :result 30))

  (ert-deftest test-asoc-docstring-examples-asoc-map ()
    "Docstring examples for `asoc-map'."
    (should-equal
     (asoc-map (lambda (k v) (cons k (when (symbolp k) v)))
               '((one . 1) (two . 4) (3 . 9) (4 . 16) (five . 25) (6 . 36)))
     :result '((one . 1) (two . 4) (3 . nil) (4 . nil) (five . 25) (6 . nil)))
    (should-equal
     (asoc-map (lambda (k v) (when (symbolp k) v))
               '((one . 1) (two . 4) (3 . 9) (4 . 16) (five . 25) (6 . 36)))
     :result '(1 4 nil nil 25 nil)))

  (ert-deftest test-asoc-docstring-examples-asoc--map ()
    "Docstring examples for `asoc--map'."
    (should-equal
     (asoc--map
         (cons (intern (concat (symbol-name key) "-squared"))
               (* value value))
       '((one . 1) (two . 2) (three . 3) (four . 4)))
     :result '((one-squared . 1) (two-squared . 4) (three-squared . 9) (four-squared . 16)))
    (should-equal
     (asoc--map (cons (intern key) value)
       '(("one" . 1) ("two" . 2) ("three" . 3)))
     :result '((one . 1) (two . 2) (three . 3)))
    (should-equal
     (asoc--map (format "%s=%d;" key value)
       '((one . 1) (two . 2) (three . 3) (four . 4)))
     :result '("one=1;" "two=2;" "three=3;" "four=4;")))

    (ert-deftest test-asoc-docstring-examples-asoc-map-keys ()
      "Docstring examples for `asoc-map-keys'."
      (should-equal
       (asoc-map-keys #'symbol-name
                      '((one . 1) (two . 4) (three . 9) (four . 16) (five . 25)))
       :result '(("one" . 1) ("two" . 4) ("three" . 9) ("four" . 16) ("five" . 25))))

  (ert-deftest test-asoc-docstring-examples-asoc-map-values ()
    "Docstring examples for `asoc-map-values'."
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))))
       (asoc-map-values #'list a))
     :result '((1 1) (2 4) (3 9) (4 16) (5 25))))

  (ert-deftest test-asoc-docstring-examples-asoc-sort-keys ()
    "Docstring examples for `asoc-sort-keys'."
    (should-equal
     (let ((a '((b . 2) (a . 1) (e . 5) (d . 4) (c . 3))))
       (asoc-sort-keys a #'string<))
     :result '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5))))

  (ert-deftest test-asoc-docstring-examples-asoc-filter ()
    "Docstring examples for `asoc-filter'."
    (should-equal
     (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
       (asoc-filter #'> fib))
     :result '((2 . 1) (3 . 2) (4 . 3))))

  (ert-deftest test-asoc-docstring-examples-asoc--filter ()
    "Docstring examples for `asoc--filter'."
    (should-equal
     (asoc--filter (not (eq key value))
       '((a . b) (b . c) (c . c) (d . a) (e . e)))
     :result '((a . b) (b . c) (d . a))))

  (ert-deftest test-asoc-docstring-examples-asoc-filter-keys ()
    "Docstring examples for `asoc-filter-keys'."
    (should-equal
     (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
       (asoc-filter-keys (lambda (k) (<= k 3)) fib))
     :result '((1 . 1) (2 . 1) (3 . 2))))

  (ert-deftest test-asoc-docstring-examples-asoc-filter-values ()
    "Docstring examples for `asoc-filter-values'."
    (should-equal
     (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
       (asoc-filter-values (lambda (v) (<= v 3)) fib))
     :result '((1 . 1) (2 . 1) (3 . 2) (4 . 3))))

  (ert-deftest test-asoc-docstring-examples-asoc-remove ()
    "Docstring examples for `asoc-remove'."
    (should-equal
     (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
       (asoc-remove #'> fib))
     :result '((1 . 1) (5 . 5) (6 . 8) (7 . 13) (8 . 21))))

  (ert-deftest test-asoc-docstring-examples-asoc-remove-keys ()
    "Docstring examples for `asoc-remove-keys'."
    (should-equal
     (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
       (asoc-remove-keys (lambda (k) (<= k 3)) fib))
     :result '((4 . 3) (5 . 5) (6 . 8) (7 . 13) (8 . 21))))

  (ert-deftest test-asoc-docstring-examples-asoc-remove-values ()
    "Docstring examples for `asoc-remove-values'."
    (should-equal
     (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
       (asoc-remove-values (lambda (v) (<= v 3)) fib))
     :result '((5 . 5) (6 . 8) (7 . 13) (8 . 21))))

  (ert-deftest test-asoc-docstring-examples-asoc-partition ()
    "Docstring examples for `asoc-partition'."
    (should-equal
     (asoc-partition '(a 1 b 2 c 3 d 4 e 5 f 6))
     :result '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6))))

  (ert-deftest test-asoc-docstring-examples-asoc-fold ()
    "Docstring examples for `asoc-fold'."
    ;; list of keys with value of 0
    (should-equal
     (let ((a '((1 . 0) (2 . 0) (3 . 0) (4 . 1) (5 . 0)
                (6 . 2) (7 . 7) (8 . 3) (9 . 2) (10 . 0))))
       (asoc-fold (lambda (k v acc) (if (zerop v) (cons k acc) acc))
                  (reverse a) nil))
     :result '(1 2 3 5 10)))

  (ert-deftest test-asoc-docstring-examples-asoc--fold ()
    "Docstring examples for `asoc--fold'."
    ;; list of keys with value of 0
    (should-equal
     (let ((a '((1 . 0) (2 . 0) (3 . 0) (4 . 1) (5 . 0)
                (6 . 2) (7 . 7) (8 . 3) (9 . 2) (10 . 0))))
       (asoc--fold (if (zerop value) (cons key acc) acc)
         (reverse a) nil))
     :result '(1 2 3 5 10)))

  (ert-deftest test-asoc-docstring-examples-asoc-uniq ()
    "Docstring examples for `asoc-uniq'."
    (should-equal
     (asoc-uniq '((a 1) (b 2) (b 3) (c 4) (a 5)))
     :result '((a 1) (b 2) (c 4))))

  (ert-deftest test-asoc-docstring-examples-asoc-merge-values ()
    "Docstring examples for `asoc-merge-values'."
    (should-equal
     (let ( (a '((a . 1) (b . 2) (a . 3) (a . 1)))
            (b '((a . 5) (b . 2) (c . 3))) )
       (asoc-merge-values a b))
     :result '((a 1 3 1 5) (b 2 2) (c 3))))

  (ert-deftest test-asoc-docstring-examples-asoc-merge-values-no-dups ()
    "Docstring examples for `asoc-merge-values-no-dups'."
    (should-equal
     (let ( (a '((a . 1) (b . 2) (a . 3) (a . 1)))
            (b '((a . 5) (b . 2) (c . 3))) )
       (asoc-merge-values-no-dups a b))
     :result '((a 1 3 5) (b 2) (c 3))))

  )

(defun asoc---test-all ()
  (interactive)
  (load-file "asoc-test.el")
  (load-file "asoc-test-docstrings.el")
  (ert-run-tests-batch "^test-asoc" ))
