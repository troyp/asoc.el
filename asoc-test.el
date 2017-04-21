;;;  -*- lexical-binding: t -*-

(require 'asoc)
(require 'ert)

(cl-macrolet ((should-equal
               (expr keyword result)
               (if (eq keyword :result)
                   `(should (equal ,expr ,result))
                 (error "expected :result"))))

  (ert-deftest test-asoc-unit-tests-asoc--compare ()
    "Unit tests for `asoc--compare'."
    (should-equal
     (let (table)
       (dolist (fn
                (list #'equalp #'equal #'eql #'eq)
                table)
         (let* ( (result (list :: fn))
                 (p  '(1 2))
                 (asoc-compare-fn fn))
           (dolist (xy
                    (list '(    3   .   3    )   ;; 1. int
                          '(    3   .   3.0  )   ;; 2. int vs float
                          '(   "a"  .  "a"   )   ;; 4. strings
                          '(   "a"  .  "A"   )   ;; 5. strings, diff case
                          '(    x   .   x    )   ;; 6. symbols
                          '(  (1 2) . (1 2)  )   ;; 3. lists
                          `(   ,p   .  ,p    )   ;; 7. same object
                          ))
             (setq result (cons (asoc--compare (car xy) (cdr xy))
                                result)))
           (setq table (cons (reverse result) table)))))
     :result
     '( ;  FN        3~3  3~3.0  "a"~"a"  "a"~"A"  x~x (1 2)~(1 2) p~p
       (  eq      ::  t    nil     nil      nil     t      nil      t  )
       (  eql     ::  t    nil     nil      nil     t      nil      t  )
       (  equal   ::  t    nil     t        nil     t      t        t  )
       (  equalp  ::  t    t       t        t       t      t        t  )))
    ;; float equality under #'eql
    (should-equal
     (let ((asoc-compare-fn #'eql))
       (asoc--compare 3.0 3.0))
     :result t)
    )

  (ert-deftest test-asoc-unit-tests-asoc--assoc ()
    "Unit tests for `asoc--assoc'."
    (should-equal
     (let* (( table  nil    )
            ( p      '(1 2) )
            ( a      `((1   . t) (2.0 . t) ("a" . t) (,p  . t) (nil . t)) )
            (test-items
             ;; TEST-ITEM ;;    ALIST-ELEM
             `( 1         ;;    1           | 1  int
                1.0       ;;    1           | 2  float matches int
                2.0       ;;    2.0         | 3  float
                2         ;;    2.0         | 4  int matches float
                "a"       ;;    "a"         | 5  string
                "A"       ;;    "a"         | 6  string, other case
                (1 2)     ;;    p = (1 2)   | 7  list, same structure
                ,p        ;;    p           | 8  list, same object
                nil       ;;    nil         | 9  nil
             )))
       (dolist (eqfn (list #'equalp #'equal #'eql #'eq))
         (let* (( result  (list :: eqfn) ))
           (dolist (test test-items)
             (setq result (cons (asoc--assoc test a eqfn)
                                result)))
           (setq table (cons (reverse result) table))))
       table
       )
     :result
     ;;  FN         1/1    1.0/1   2.0/2.0    2/2.0    "a"/"a"   "A"/"a"  (1 2)/(1 2) (1 2),same     nil
     '((eq     :: (1 . t)   nil      nil       nil       nil       nil        nil     ((1 2) . t) (nil . t))
       (eql    :: (1 . t)   nil   (2.0 . t)    nil       nil       nil        nil     ((1 2) . t) (nil . t))
       (equal  :: (1 . t)   nil   (2.0 . t)    nil    ("a" . t)    nil    ((1 2) . t) ((1 2) . t) (nil . t))
       (equalp :: (1 . t) (1 . t) (2.0 . t) (2.0 . t) ("a" . t) ("a" . t) ((1 2) . t) ((1 2) . t) (nil . t)))
     )
    )

  (ert-deftest test-asoc-unit-tests-asoc--member ()
    "Unit tests for `asoc--member'."
    (should-equal
     (let* (( table  nil     )
            ( p      '(1 2)  )
            ;; l        8 7 6    5  4   3  2  1     | length of tail from elt
            ( l      `( 1 2 3.0 ,p "a" 'c nil t ))
            (test-items
             ;; TEST-ITEM   ;;  LIST-ELEM
             `( 2           ;;    2       | 1   ints
                2.0         ;;    2       | 2   int elt, float test item
                3           ;;    3.0     | 3   float elt, int test item
                3.0         ;;    3.0     | 4   floats
                ,p          ;;    p       | 5   list, same object
                (1 2)       ;;    p       | 6   lists with same elts, different objects
                "a"         ;;    "a"     | 7   letters
                "A"         ;;    "a"     | 8   letters, opposite case
                c           ;;    c       | 9   symbols
                nil         ;;    nil     | 10  nil
                t           ;;    t       | 11  t
                )))
       (dolist (f (list #'equalp #'equal #'eql #'eq))
         (let* (( result           (list :: f) )
                ( asoc-compare-fn       f       ) )
           (dolist (test test-items)
             (let ((ltail (asoc--member test l)))
               (setq result (cons (when ltail (length ltail))
                                  result))))
           (setq table (cons (reverse result) table))))
       table
       )
     :result
     ;;  Integers represent the length of the tail starting with TEST-ITEM
     ;;  LIST ITEM:  2  2    3.0  3.0  ,p   ,p      "a"  "a" 'c   nil  t
     ;;  TEST ITEM:  2  2.0  3    3.0  ,p   (1  2)  "a"  "A"  c   nil  t
     '((eq        ::  7  nil  nil  nil  5    nil     nil  nil  nil  2   1 )
       (eql       ::  7  nil  nil  6    5    nil     nil  nil  nil  2   1 )
       (equal     ::  7  nil  nil  6    5    5       4    nil  nil  2   1 )
       (equalp    ::  7  7    6    6    5    5       4    4    nil  2   1 ))
     )
    )

  (ert-deftest test-asoc-unit-tests-asoc-make ()
    "Unit tests for `asoc-make'."
    ;; no args
    (should-equal (asoc-make) :result nil)
    ;; with keys
    (should-equal (asoc-make '(a b c d))
                  :result '((a) (b) (c) (d)))
    (should-equal (asoc-make '(a b c d) nil)
                  :result '((a) (b) (c) (d)))
    ;; with default
    (should-equal
     (asoc-make '(a b c d) 'undefined)
     :result '((a . undefined) (b . undefined) (c . undefined) (d . undefined)))
    )

  (ert-deftest test-asoc-unit-tests-asoc-contains-key? ()
    "Unit tests for `asoc--contains-key?'."
    (should-equal
     (let* (( table  nil    )
            ( p      '(1 2) )
            ( a      `((1   . t) (2.0 . t) ("a" . t) (,p  . t) (nil . t)) )
            (test-items
             ;; TEST-ITEM ;;    ALIST-ELEM
             `( 1         ;;    1           | 1  int
                1.0       ;;    1           | 2  float matches int
                2.0       ;;    2.0         | 3  float
                2         ;;    2.0         | 4  int matches float
                "a"       ;;    "a"         | 5  string
                "A"       ;;    "a"         | 6  string, other case
                (1 2)     ;;    p = (1 2)   | 7  list, same structure
                ,p        ;;    p           | 8  list, same object
                nil       ;;    nil         | 9  nil
             )))
       (dolist (fn (list #'equalp #'equal #'eql #'eq))
         (let* (( result           (list :: fn) )
                ( asoc-compare-fn  fn  ))
           (dolist (test test-items)
             (setq result (cons (asoc-contains-key? a test)
                                result)))
           (setq table (cons (reverse result) table))))
       table
       )
     :result
     ;;  FN        1/1  1.0/1  2.0/2.0  2/2.0 "a"/"a" "A"/"a" (1 2)/(1 2) (1 2),same nil
     `(( eq     ::  t    nil     nil     nil    nil     nil       nil          t      t )
       ( eql    ::  t    nil      t      nil    nil     nil       nil          t      t )
       ( equal  ::  t    nil      t      nil     t      nil        t           t      t )
       ( equalp ::  t    t        t       t      t       t         t           t      t ))
     )
    )

  (ert-deftest test-asoc-unit-tests-asoc-contains-pair? ()
    "Unit tests for `asoc-contains-pair?'"
    (should-equal
     (let* (( table  nil     )
            ( p      '(1 2)  )
            ( q      '(1 . 2))
            ( a      `( ,q (1 . 3.0) (1 . "a") (1 . ,p) ,p) )
            (test-items
             ;; TEST-ITEM ;;    ALIST-ELEM
             `( (1 . 2)     ;;    q = (1 . 2)       | 1  ints
                ,q          ;;    q                 | 2  ints, same cons
                (1 . 3)     ;;    (1 . 3.0)         | 3  int value matches float
                (1 . 3.0)   ;;    (1 . 3.0)         | 4  int keys, float values
                (1 . "a")   ;;    (1 . "a")         | 5  int keys, string values
                (1 . "A")   ;;    (1 . "a")         | 6  int keys, string value opp case
                (1 . (1 2)) ;;    (1 . p=(1 2))     | 7  list values
                (1 . ,p)    ;;    (1 . p=(1 2))     | 8  list values (values are the same object)
                (1 . (2))   ;;    p=(1 2)=(1 . (2)) | 9  list values
                ,p          ;;    p                 | 10 list values, same cons
                )))
       (dolist (fn (list #'equalp #'equal #'eql #'eq))
         (let* (( result           (list :: fn) )
                ( asoc-compare-fn  fn  ))
           (dolist (test test-items)
             (setq result (cons (asoc-contains-pair? a (car test) (cdr test))
                                result)))
           (setq table (cons (reverse result) table))))
       table
       )
     :result
     ;; ALIST ITEM:    q    q  (1 . 3.0) (1 . 3.0)  (1 . "a")  (1 . "a")  (1 .   p)   (1 . p)    p   p
     ;;  TEST ITEM: (1 . 2) q  (1 . 3)   (1 . 3.0)  (1 . "a")  (1 . "A")  (1 . (1 2)) (1 . p)  (1 2) p
     '(( eq      ::    t    t    nil       nil        nil        nil        nil          t     nil   t )
       ( eql     ::    t    t    nil        t         nil        nil        nil          t     nil   t )
       ( equal   ::    t    t    nil        t          t         nil         t           t      t    t )
       ( equalp  ::    t    t     t         t          t          t          t           t      t    t ))
     )
    )

  (ert-deftest ert-deftest-unit-tests-asoc-get ()
    "Unit-tests for `asoc-get'."
    (should-equal
     (let* (( table  nil    )
            ( p      '(1 2) )
            ( a      `((1   . 1) (2.0 . 2) ("a" . 3) (,p  . 4) (nil . 5)) )
            (test-items
             ;; TEST-ITEM ;;    ALIST-KEY
             `( 1         ;;    1           | 1  int
                1.0       ;;    1           | 2  float matches int
                2.0       ;;    2.0         | 3  float
                2         ;;    2.0         | 4  int matches float
                "a"       ;;    "a"         | 5  string
                "A"       ;;    "a"         | 6  string, other case
                (1 2)     ;;    p = (1 2)   | 7  list, same structure
                ,p        ;;    p           | 8  list, same object
                nil       ;;    nil         | 9  nil
                )))
       (dolist (fn (list #'equalp #'equal #'eql #'eq))
         (let* (( result           (list :: fn) )
                ( asoc-compare-fn  fn  ))
           (dolist (test test-items)
             (setq result (cons (asoc-get a test)
                                result)))
           (setq table (cons (reverse result) table))))
       table
       )
     :result
     ;;  FN        1/1  1.0/1  2.0/2.0  2/2.0 "a"/"a" "A"/"a" (1 2)/(1 2) (1 2),same nil
     '(( eq     ::  1    nil     nil     nil    nil     nil       nil          4      5 )
       ( eql    ::  1    nil      2      nil    nil     nil       nil          4      5 )
       ( equal  ::  1    nil      2      nil     3      nil        4           4      5 )
       ( equalp ::  1     1       2       2      3       3         4           4      5 ))
     )
    ;; empty alist
    (should-equal (asoc-get nil 1) :result nil)
    (should-equal (asoc-get nil nil) :result nil)
    )

  (ert-deftest test-asoc-unit-tests-asoc-put! ()
    "Unit tests for `asoc-put!'."
      ;; test with replace=nil
      (should-equal
       (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))))
         (asoc-put! a 3 10))
       :result '((3 . 10) (1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))
      ;; test with replace=non-nil
      (should-equal
       (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))))
         (asoc-put! a 3 10 :replace))
       :result '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25)))
      ;; test with replace=non-nil, multiple deletions
      (should-equal
       (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (3 . 1) (5 . 25))))
         (asoc-put! a 3 10 :replace))
       :result '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25)))
      ;; test with replace=non-nil, no deletions
      (should-equal
       (let ((a '((1 . 1) (2 . 4) (4 . 16) (5 . 25))))
         (asoc-put! a 3 10 :replace))
       :result '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25)))
      ;; test with replace=non-nil, deletion at head of list
      (should-equal
       (let ((a '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25))))
         (asoc-put! a 3 10 :replace))
       :result '((3 . 10) (1 . 1) (2 . 4) (4 . 16) (5 . 25)))
      ;; empty alist
      (should-equal
       (let ((a nil)) (asoc-put! a 3 10))
       :result '((3 . 10)))
      ;; one-pair alist
      (should-equal
       (let ((a '((3 . 10)))) (asoc-put! a 3 10))
       :result '((3 . 10) (3 . 10)))
      (should-equal
       (let ((a '((3 . 10)))) (asoc-put! a 3 10 :replace))
       :result '((3 . 10)))
      )

  (ert-deftest test-asoc-unit-tests-asoc-remove! ()
    "Unit tests for `asoc-remove!'."
    ;; test nil, 1-pair (match/non-match), 2-pairs (match/non)
    (should-equal
     (let ((a nil))
       (asoc-remove! a 'x))
     :result nil)
    (should-equal
     (let ((a '((x 1))))
       (asoc-remove! a 'x))
     :result nil)
    (should-equal
     (let ((a '((y 2))))
       (asoc-remove! a 'x))
     :result '((y 2)))
    (should-equal
     (let ((a '((x 1) (y 2))))
       (asoc-remove! a 'x))
     :result '((y 2)))
    (should-equal
     (let ((a '((y 2) (x 1))))
       (asoc-remove! a 'x))
     :result '((y 2)))
    (should-equal
     (let ((a '((x 1) (x 11))))
       (asoc-remove! a 'x))
     :result '((x 11)))
    (should-equal
     (let ((a '((x 1) (x 11) (x 111))))
       (asoc-remove! a 'x))
     :result '((x 11) (x 111)))
    )

  (ert-deftest test-asoc-unit-tests-asoc-remove!/remove-all ()
    "Unit tests for `asoc-remove!' with remove-all set."
    ;; test nil, 1-pair (match/non-match), 2-pairs (match/non)
    (should-equal
     (let ((a nil))
       (asoc-remove! a 'x :all))
       :result nil)
    (should-equal
     (let ((a '((x 1))))
       (asoc-remove! a 'x :all))
     :result nil)
    (should-equal
     (let ((a '((y 2))))
       (asoc-remove! a 'x :all))
     :result '((y 2)))
    (should-equal
     (let ((a '((x 1) (y 2))))
       (asoc-remove! a 'x :all))
     :result '((y 2)))
    (should-equal
     (let ((a '((y 2) (x 1))))
       (asoc-remove! a 'x :all))
     :result '((y 2)))
    (should-equal
     (let ((a '((x 1) (x 11))))
       (asoc-remove! a 'x :all))
     :result nil)
    (should-equal
     (let ((a '((x 1) (x 11) (x 111))))
       (asoc-remove! a 'x :all))
     :result nil)
    (should-equal
     (let ((a '((x . 1) (y . 2) (x . 11) (z . 3) (x . 111) (x . 1111))))
       (asoc-remove! a 'x :all))
     :result '((y . 2) (z . 3)))
    (should-equal
     (let ((a '((x . 1) (y . 2) (x . 11) (z . 3) (x . 111) (x . 1111) (z . 33))))
       (asoc-remove! a 'x :all))
     :result '((y . 2) (z . 3) (z . 33)))
    )


  (ert-deftest test-asoc-unit-tests-asoc-keys ()
    "Unit tests for `asoc-keys'."
    ;; empty alist
    (should-equal
     (let ((a nil))
       (asoc-keys a))
     :result nil)
    ;; alist with duplicates
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (1 . 1) (3 . 9) (1 . 1) (4 . 16) (5 . 25))))
       (asoc-keys a))
     :result '(1 2 3 4 5))
    ;; test with different choices of asoc-compare-fn
    (should-equal
     (let* (( p      '(1 2) )
            ( a      `((1   . nil) ("a" . nil) (,p  . nil)   (nil . nil)
                       (1.0 . nil) ("a" . nil) ((1 2) . nil) (nil . nil) ))
            (result))

       (dolist (fn (list #'equalp #'equal #'eql #'eq))
         (let* (( asoc-compare-fn  fn  ))
           (setq result (cons (list fn :: (asoc-keys a))
                              result))))
       result)
     :result   ;; <-first occurrences->         <-duplicates->
     '((eq     ::  (1  "a"  (1 2)  nil          1.0  "a" (1 2) ))
       (eql    ::  (1  "a"  (1 2)  nil          1.0  "a" (1 2) ))
       (equal  ::  (1  "a"  (1 2)  nil          1.0            ))
       (equalp ::  (1  "a"  (1 2)  nil                         )))
     )
    )

  (ert-deftest test-asoc-unit-tests-asoc-do ()
    "Unit tests for `asoc-do'."
    ;; error if the variable RESULT is not defined
    (should-error
     (let ((a '((one . 1) (two . 4) (3 . 9) (4 . 16) (five . 25) (6 . 36))))
       (let (sum)
         (makunbound 'sum)
         (asoc-do ((key value) a sum)
           (when (symbolp key)
             (setf sum (+ sum value)))))))
    )

  (ert-deftest test-asoc-unit-tests-asoc-map-values ()
    "Unit tests for `asoc-map-values'."
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))))
       (asoc-map-values (lambda (x) (* x x)) a))
     :result '((1 . 1) (2 . 16) (3 . 81) (4 . 256) (5 . 625)))
    ;; empty alist
    (should-equal
     (let ((a nil))
       (asoc-map-values (lambda (x) (* x x)) a))
    :result nil)
    )

  (ert-deftest test-asoc-unit-tests-asoc-zip ()
    "Unit tests for `asoc-zip'."
    ;; #keys == #values
    (should-equal
     (asoc-zip '(1 2 3 4 5) '(1 4 9 16 25))
     :result '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))
    ;; #keys > #values
    (should-equal
     (asoc-zip '(1 2 3 4 5 6 7) '(1 4 9 16 25))
     :result '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25) (6) (7)))
    ;; #keys < #values
    (should-error
     (asoc-zip '(1 2 3 4 5) '(1 4 9 16 25 36)))
    ;; empty list
    (should-equal
     (asoc-zip nil nil)
     :result nil)
    ;; empty values
    (should-equal
     (asoc-zip '(1 2 3 4 5) nil)
     :result '((1) (2) (3) (4) (5)))
    ;; values sequence is a string
    (should-equal
     (asoc-zip '(1 2 3 4 5 6) "qwerty")
     :result '((1 . ?q) (2 . ?w) (3 . ?e) (4 . ?r) (5 . ?t) (6 . ?y)))
    )

  (ert-deftest test-asoc-unit-tests-asoc-uniq ()
    "Unit tests for `asoc-uniq'."
    ;; empty list
    (should-equal
     (asoc-uniq nil)
    :result nil)
    ;; 1-element list
    (should-equal
     (asoc-uniq '((1 1)))
     :result '((1 1)))
    ;; list with multiple repeats
    (should-equal
     (asoc-uniq '((1 1) (2 2) (1 3) (1 4) (1 5) (2 3)))
     :result '((1 1) (2 2)))
    (should-equal
     (let* (( result   nil    )
            ( p       '(1 2)  )
            ( a       `( (1 1)   (,p 1)    ("a" 1) ('c 1) (nil 1) (t 1)
                         (1 2)   (,p 2)    ("a" 2) ('c 2) (nil 2) (t 2)
                         (1.0 3) ((1 2) 3) ("A" 3)                      )))
       (dolist (f (list #'equalp #'equal #'eql #'eq))
         (let (( asoc-compare-fn f ))
           (setq result (cons (list f ::: (asoc-uniq a))
                              result))))
       (reverse result))
     :result
     '((equalp ::: ( (1 1) ((1 2) 1) ("a" 1) ('c 1) (nil 1) (t 1)                                          ) )
       (equal  ::: ( (1 1) ((1 2) 1) ("a" 1) ('c 1) (nil 1) (t 1)                (1.0 3)           ("A" 3) ) )
       (eql    ::: ( (1 1) ((1 2) 1) ("a" 1) ('c 1) (nil 1) (t 1) ("a" 2) ('c 2) (1.0 3) ((1 2) 3) ("A" 3) ) )
       (eq     ::: ( (1 1) ((1 2) 1) ("a" 1) ('c 1) (nil 1) (t 1) ("a" 2) ('c 2) (1.0 3) ((1 2) 3) ("A" 3) ) ))
     )
    ;; Duplicate floating point keys are removed with #'equalp, #'equal and #'eql
    (should-equal
     (list
      (append '(equalp :::)
              (let ((asoc-compare-fn #'equalp))
                (asoc-uniq '((1.0 1) (1.0 2)))))
      (append '(equal :::)
              (let ((asoc-compare-fn #'equal))
                (asoc-uniq '((1.0 1) (1.0 2)))))
      (append '(eql :::)
              (let ((asoc-compare-fn #'eql))
                (asoc-uniq '((1.0 1) (1.0 2)))))
      )
     :result
     '((equalp ::: (1.0 1))
       (equal  ::: (1.0 1))
       (eql    ::: (1.0 1)))
     )
    )

  ;; ,-----------------,
  ;; | Docstring Tests |
  ;; '-----------------'

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

  (ert-deftest test-asoc-docstring-examples-asoc-map-values ()
    "Docstring examples for `asoc-map-values'."
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))))
       (asoc-map-values #'list a))
     :result '((1 1) (2 4) (3 9) (4 16) (5 25))))

  (ert-deftest test-asoc-docstring-examples-asoc-filter ()
    "Docstring examples for `asoc-map-values'."
    (should-equal
     (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
       (asoc-filter #'> fib))
     :result '((2 . 1) (3 . 2) (4 . 3))))

  (ert-deftest test-asoc-docstring-examples-asoc-fold ()
    "Docstring examples for `asoc-fold'."
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))
           (s ""))
       (asoc-fold (lambda (k v acc)
                    (concat acc (format "%S\t%S\n" k v)))
                  a ""))
     :result "1\t1\n2\t4\n3\t9\n4\t16\n5\t25\n"))

  (ert-deftest test-asoc-docstring-examples-asoc-uniq ()
    "Docstring examples for `asoc-uniq'."
    (should-equal
     (asoc-uniq '((a 1) (c 6) (b 2) (c 3) (d 4)))
     :result '((a 1) (c 6) (b 2) (d 4))
     ))

  )

(defun asoc---test-all ()
  (interactive)
  (ert-run-tests-batch "^test-asoc" ))
