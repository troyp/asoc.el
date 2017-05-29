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

  (ert-deftest test-asoc-unit-tests-asoc---compare ()
    "Unit tests for `asoc---compare'."
    (should-equal
     (let ( table )
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
             (push (asoc---compare (car xy) (cdr xy))
                   result))
           (push (reverse result) table))))
     :result
     '( ;  FN        3~3  3~3.0  "a"~"a"  "a"~"A"  x~x (1 2)~(1 2) p~p
       (  eq      ::  t    nil     nil      nil     t      nil      t  )
       (  eql     ::  t    nil     nil      nil     t      nil      t  )
       (  equal   ::  t    nil     t        nil     t      t        t  )
       (  equalp  ::  t    t       t        t       t      t        t  )))
    ;; float equality under #'eql
    (should-equal
     (let ((asoc-compare-fn #'eql))
       (asoc---compare 3.0 3.0))
     :result t)
    )

  (ert-deftest test-asoc-unit-tests-asoc---assoc ()
    "Unit tests for `asoc---assoc'."
    (should-equal
     (let* (  table
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
             (push (asoc---assoc test a eqfn)
                   result))
           (push (reverse result) table)))
       table)
     :result
     ;;  FN         1/1    1.0/1   2.0/2.0    2/2.0    "a"/"a"   "A"/"a"  (1 2)/(1 2) (1 2),same     nil
     '((eq     :: (1 . t)   nil      nil       nil       nil       nil        nil     ((1 2) . t) (nil . t))
       (eql    :: (1 . t)   nil   (2.0 . t)    nil       nil       nil        nil     ((1 2) . t) (nil . t))
       (equal  :: (1 . t)   nil   (2.0 . t)    nil    ("a" . t)    nil    ((1 2) . t) ((1 2) . t) (nil . t))
       (equalp :: (1 . t) (1 . t) (2.0 . t) (2.0 . t) ("a" . t) ("a" . t) ((1 2) . t) ((1 2) . t) (nil . t)))
     )
    )

  (ert-deftest test-asoc-unit-tests-asoc---uniq ()
    "Unit tests for `asoc---uniq'."
    ;; null list
    (should-equal
     (asoc---uniq nil)
     :result nil)
    ;; unique keys
    (let ((alist (asoc-zip (number-sequence 1 10) (number-sequence 2 20 2))))
      (should-equal
       (asoc---uniq alist)
       :result alist))
    ;; duplicate keys
    (should-equal
     (let ((alist '((a . 1) (a . 2) (a . 3) (a . 4))))
       (asoc---uniq alist))
     :result '((a . 1)))
    ;; choice of asoc-compare-fn
    (should-equal
     (let* (( p      '(1 2) )
            ( a      `((1   . 1) ("a" . 1) (,p    . 1) (nil . 1)
                       (1.0 . 2) ("A" . 2) ((1 2) . 2) (nil . 2)
                                 ("a" . 3)                      ))
            (result))
       (dolist (fn (list #'equalp #'equal #'eql #'eq))
         (let* (( asoc-compare-fn  fn  ))
           (push (list fn :: (asoc---uniq a))
                 result)))
       result)
     :result    ;; <-----------first occurrences----------> <---------------duplicates--------------->
     '( (eq     :: ((1 . 1) ("a" . 1) ((1 2) . 1) (nil . 1) (1.0 . 2) ("A" . 2) ((1 2) . 2) ("a" . 3)) )
        (eql    :: ((1 . 1) ("a" . 1) ((1 2) . 1) (nil . 1) (1.0 . 2) ("A" . 2) ((1 2) . 2) ("a" . 3)) )
        (equal  :: ((1 . 1) ("a" . 1) ((1 2) . 1) (nil . 1) (1.0 . 2) ("A" . 2)                      ) )
        (equalp :: ((1 . 1) ("a" . 1) ((1 2) . 1) (nil . 1)                                            ) ))
     )
    )

  (ert-deftest test-asoc-unit-tests-asoc---list-member ()
    "Unit tests for `asoc---list-member'."
    (should-equal
     (let* (  table
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
             (let ((ltail (asoc---list-member test l)))
               (push (when ltail (length ltail))
                     result)))
           (push (reverse result) table)))
       table)
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

  (ert-deftest test-asoc-unit-tests-asoc---list-filter ()
    "Unit tests for `asoc---list-filter'."
    ;; numeric predicate
    (should-equal
     (let ((list (number-sequence 1 10)))
       (asoc---list-filter (lambda (n) (zerop (% n 3)))
                           list))
     :result '(3 6 9))
    ;; returns original list with constant t predicate
    (should-equal
     (let* ( results
             (lists '( nil
                       (1 2 3 4)
                       (nil t)   )) )
       (dolist (list lists)
         (let ((result (asoc---list-filter (lambda (x) t)
                                           list)))
           (push (cons list result) results)))
       (reverse results))
     :result
     '( ( nil       . nil       )
        ( (1 2 3 4) . (1 2 3 4) )
        ( (nil t)   . (nil t))  )
     )
    ;; check interned DEL symbols are not removed
    (should-equal
     (let ((list '(DEL DEL)))
       (asoc---list-filter #'symbolp list))
     :result '(DEL DEL))
    ;; check nil is not removed
    (should-equal
     (let ((list '(nil nil)))
       (asoc---list-filter #'symbolp list))
     :result '(nil nil))
    )

  (ert-deftest test-asoc-unit-tests-asoc---list-remove ()
    "Unit tests for `asoc---list-remove'."
    ;; numeric predicate
    (should-equal
     (let ((list (number-sequence 1 10)))
       (asoc---list-remove (lambda (n) (zerop (% n 3)))
                           list))
     :result '(1 2 4 5 7 8 10))
    ;; returns original list with constant nil predicate
    (should-equal
     (let* ((results nil)
            (lists '(
                     nil
                     (1 2 3 4)
                     (nil t)
                     )))
       (dolist (list lists)
         (let ((result (asoc---list-remove (lambda (x) nil)
                                           list)))
           (push (cons list result) results)))
       (reverse results))
     :result
     '(( nil       . nil       )
       ( (1 2 3 4) . (1 2 3 4) )
       ( (nil t)   . (nil t))  )
     )
    ;; check interned DEL symbols are not removed
    (should-equal
     (let ((list '(DEL DEL)))
       (asoc---list-remove #'integerp list))
     :result '(DEL DEL)
     )
    ;; check nil is not removed
    (should-equal
     (let ((list '(nil nil)))
       (asoc---list-remove #'integerp list))
     :result '(nil nil))
    )

  (ert-deftest test-asoc-unit-tests-asoc---list-take ()
    "Unit tests for `asoc---list-take'."
    ;; empty list
    (should-equal (asoc---list-take 0 nil) :result nil)
    (should-equal (asoc---list-take 1 nil) :result nil)
    (should-equal (asoc---list-take 5 nil) :result nil)
    ;; one element
    (should-equal (asoc---list-take 0 '(a)) :result nil)
    (should-equal (asoc---list-take 1 '(a)) :result '(a))
    (should-equal (asoc---list-take 5 '(a)) :result '(a))
    ;; many elements
    (let ((alist (number-sequence 1 10)))
      (should-equal (asoc---list-take 0 alist) :result nil)
      (should-equal (asoc---list-take 1 alist) :result '(1))
      (should-equal (asoc---list-take 5 alist) :result '(1 2 3 4 5)))
    ;; 1st argument not a number/marker
    (should-error (asoc---list-take nil nil) :type 'wrong-type-argument)
    ;; 2nd argument not a list
    (should-error (asoc---list-take 5 5) :type 'wrong-type-argument)
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

  (ert-deftest test-asoc-unit-tests-asoc-merge ()
    "Unit tests for `asoc-merge'."
    ;; no list
    (should-equal
     (asoc-merge)
     :result nil)
    ;; null lists
    (should-equal
     (asoc-merge nil nil nil)
     :result nil)
    ;; last alist takes precedence
    (should-equal
     (let ((a1 '((x . 1)        ))
           (a2 '((x . 2)        ))
           (a3 '((x . 3) (y . 3)))
           (a4 '(        (y . 4))))
       (asoc-merge a1 a2 a3 a4))
     :result '((y . 4) (x . 3)))
     ;; first pair in alist takes precedence
    (should-equal
     (asoc-merge '((a . 1) (b . 1) (b . 2) (a . 2) (a . 3) (c . 1)))
     :result '((a . 1) (b . 1) (c . 1)))
    ;; multiple alists with repeated keys
    (should-equal
     (let ((a '((a . 1) (a . 2) (b . 1) (c . 1)))
           (b '((a . 3) (b . 2) (c . 2) (d . 1)))
           (c '((e . 1) (e . 2) (f . 1) (g . 1))))
       (asoc-merge a b c))
     :result '((e . 1) (f . 1) (g . 1) (a . 3) (b . 2) (c . 2) (d . 1)))
    )

  (ert-deftest test-asoc-unit-tests-asoc-uniq ()
    "Unit tests for `asoc-uniq'."
    ;; empty list
    (should-equal
     (asoc-uniq nil)
     :result nil)
    ;; 1-element list
    (should-equal
     (asoc-uniq '((1 . 1)))
     :result '((1 . 1)))
    ;; list with multiple repeats
    (should-equal
     (asoc-uniq '((1 . 1) (2 . 2) (1 . 3) (1 . 4) (1 . 5) (2 . 3)))
     :result '((1 . 1) (2 . 2)))
    (should-equal
     (let* (( result   nil    )
            ( p       '(1 2)  )
            ( a       `( (1 1)   (,p 1)    ("a" 1) ('c 1) (nil 1) (t 1)
                         (1 2)   (,p 2)    ("a" 2) ('c 2) (nil 2) (t 2)
                         (1.0 3) ((1 2) 3) ("A" 3)                      )))
       (dolist (f (list #'equalp #'equal #'eql #'eq))
         (let (( asoc-compare-fn f ))
           (push (list f ::: (asoc-uniq a))
                 result)))
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
                (asoc-uniq '((1.0 . 1) (1.0 . 2)))))
      (append '(equal :::)
              (let ((asoc-compare-fn #'equal))
                (asoc-uniq '((1.0 . 1) (1.0 . 2)))))
      (append '(eql :::)
              (let ((asoc-compare-fn #'eql))
                (asoc-uniq '((1.0 . 1) (1.0 . 2))))))
     :result
     '((equalp ::: (1.0 . 1))
       (equal  ::: (1.0 . 1))
       (eql    ::: (1.0 . 1))))
    )

  (ert-deftest test-asoc-unit-tests-asoc-sort-keys ()
    "Unit tests for `asoc-sort-keys'."
    ;; null list
    (should-equal
     (asoc-sort-keys nil #'<)
     :result nil)
    ;; sort numeric
    (should-equal
     (let ((a '((5 . t) (3 . t) (1 . t) (2 . t) (4 . t))))
       (asoc-sort-keys a #'<))
     :result '((1 . t) (2 . t) (3 . t) (4 . t) (5 . t)))
    ;; sort symbolic
    (should-equal
     (let ((a '((b . t) (a . t) (e . t) (d . t) (c . t))))
       (asoc-sort-keys a #'string<))
     :result '((a . t) (b . t) (c . t) (d . t) (e . t)))
    ;; stability
    (should-equal
     (let ((a '((a . 2) (a . 1) (a . 3))))
       (asoc-sort-keys a #'string<))
     :result '((a . 2) (a . 1) (a . 3)))
    (should-equal
     (let ((a '((1 . a) (2 . c) (1 . b) (2 . b) (1 . c) (2 . a))))
       (asoc-sort-keys a #'<))
     :result '((1 . a) (1 . b) (1 . c) (2 . c) (2 . b) (2 . a)))
    ;; constant comparator: nil - alist unchanged
    (should-equal
     (let ((a '((b . 1) (d . 2) (c . 3) (a . 4) (e . 5))))
       (asoc-sort-keys a (lambda (x y) nil)))
     :result '((b . 1) (d . 2) (c . 3) (a . 4) (e . 5)))
    ;; constant comparator: t - alist reversed
    (should-equal
     (let ((a '((b . 1) (d . 2) (c . 3) (a . 4) (e . 5))))
       (asoc-sort-keys a (lambda (x y) t)))
     :result '((e . 5) (a . 4) (c . 3) (d . 2) (b . 1)))
    )

  (ert-deftest test-asoc-unit-tests-asoc-filter ()
    "Unit tests for `asoc-filter'."
    ;; empty list
    (should-equal
     (let ( result
            (preds '( (lambda (k v) t)
                      (lambda (k v) nil) )) )
       (dolist (pred preds)
         (push (asoc-filter pred nil)
               result))
       result)
     :result '(nil nil))
    ;; empty list, predicates with wrong number of arguments
    (should-equal
     (let ( result
            (preds '( symbolp
                      (lambda (a b c d e) t) )) )
       (dolist (pred preds)
         (push (asoc-filter pred nil)
               result))
       result)
     :result '(nil nil))
    ;; constant true and false functions
    (should-equal
     (let ( table
            (preds '(( truefn  . (lambda (k v) t))
                     ( falsefn . (lambda (k v) nil))))
            (alists '(nil
                      ((x . t))
                      ((x . t) (x . t))
                      ((x . t) (y . t) (z . t)))) )
       (dolist (fnpair preds)
         (let ( result
                (fn      (cdr fnpair))
                (fn-name (car fnpair)) )
           (dolist (alist alists)
             (push (asoc-filter fn alist)
                   result))
           (push (list fn-name ::: (reverse result)) table)))
       (reverse table))
     :result
     '(( truefn  ::: ( ( )  ((x . t))  ((x . t) (x . t))  ((x . t) (y . t) (z . t)) ))
       ( falsefn ::: ( ( )  ( )        ( )                ( )                       )))
     )
    ;; sample functions and alists
    (should-equal
     (let ((alist '((1 . 1) (2 . 4) (3 . 3) (4 . 2) (5 . 1) (6 . 7))))
       (asoc-filter #'eq alist))
     :result '((1 . 1) (3 . 3)))
    (should-equal
     (let ((alist '((a . 1) (b . nil) (c . 3) (nil . 4) (nil . nil))))
       (asoc-filter (lambda (k v) (and k v)) alist))
     :result '((a . 1) (c . 3)))
    (should-equal
     (let* ((nseq  (number-sequence 0 9))
            (alist (asoc-zip nseq
                             (mapcar (lambda (n) (- (* n n n) (* 13 n n))) nseq))))
       (asoc-filter
        (lambda (k v) (> (+ (* 51 k) v -57) 0))
        alist))
     :result '((2 . -44) (3 . -90) (4 . -144) (7 . -294) (8 . -320) (9 . -324)))
    )

  (ert-deftest test-asoc-unit-tests-asoc--filter ()
    "Unit tests for `asoc--filter'."
    ;; empty list
    (should-equal (asoc--filter t nil) :result nil)
    ;; constant true and false forms
    (should-equal
     (let ( table t-resultlist nil-resultlist
            (alists '(nil
                      ((x . t))
                      ((x . t) (x . t))
                      ((x . t) (y . t) (z . t)))))
       (dolist (alist alists)
         (push (asoc--filter t alist) t-resultlist)
         (push (asoc--filter nil alist) nil-resultlist))
       (push (list 't  ::: (reverse t-resultlist)) table)
       (push (list nil ::: (reverse nil-resultlist)) table)
       (reverse table))
     :result '( (t :::   ( nil  ((x . t))  ((x . t) (x . t))  ((x . t) (y . t) (z . t)) ))
                (nil ::: ( nil     nil            nil                    nil            )) ))
    ;; sample forms and alists
    (should-equal
     (asoc--filter value    ;; filter non-nil values
       '((a . 1) (b . nil) (c . 3) (d . 4) (e . nil) (f . nil) (g . 7)))
     :result '((a . 1) (c . 3) (d . 4) (g . 7)))
    (let ((f1 (lambda () 'dummy-function))
          (f2 (lambda () 'dummy-function)))
      (should-equal
       (asoc--filter (and (symbolp key) (functionp value))
         `(("a" . ,f1) (b . x) (c . ,f1) ("d" . y) (e . ,f2) (f . nil)))
       :result `((c . ,f1) (e . ,f2))))
    )

  (ert-deftest test-asoc-unit-tests-asoc-filter-keys ()
    "Unit tests for `asoc-filter-keys'."
    ;; empty list
    (should-equal
     (let ( result
            (preds  '((lambda (k) t)
                      (lambda (k) nil))) )
       (dolist (pred preds)
         (push (asoc-filter-keys pred nil)
               result))
       result)
     :result '(nil nil))
    ;; empty list, predicates with wrong number of arguments
    (should-equal
     (let ( result
            (preds   '(<
                       (lambda (a b c d e) t))) )
       (dolist (pred preds)
         (push (asoc-filter pred nil)
               result))
       result)
     :result '(nil nil))
    ;; constant true and false functions
    (should-equal
     (let ( table
            (preds  '(( truefn  . (lambda (k) t))
                      ( falsefn . (lambda (k) nil))))
            (alists '(nil
                      ((x . t))
                      ((x . t) (x . t))
                      ((x . t) (y . t) (z . t)))) )
       (dolist (fnpair preds)
         (let ( result
                (fn      (cdr fnpair))
                (fn-name (car fnpair)) )
           (dolist (alist alists)
             (push (asoc-filter-keys fn alist)
                   result))
           (push (list fn-name ::: (reverse result)) table)))
       (reverse table))
     :result
     '(( truefn  ::: ( ( )  ((x . t))  ((x . t) (x . t))  ((x . t) (y . t) (z . t)) ))
       ( falsefn ::: ( ( )  ( )        ( )                ( )                       )))
     )
    ;; sample functions and alists
    (should-equal
     (let ((alist '((1 . 1) (2 . 4) (3 . 3) (4 . 2) (5 . 1) (6 . 7))))
       (asoc-filter-keys (lambda (k) (< k 4))
                         alist))
     :result '((1 . 1) (2 . 4) (3 . 3)))
    (should-equal
     (let ((alist '((a . 1) (b . nil) (c . 3) (nil . 4) (nil . nil))))
       (asoc-filter-keys #'identity alist))
     :result '((a . 1) (b . nil) (c . 3)))
    )

  (ert-deftest test-asoc-unit-tests-asoc-filter-values ()
    "Unit tests for `asoc-filter-values'."
    ;; empty list
    (should-equal
     (let ( result
            (preds '( (lambda (v) t)
                      (lambda (v) nil) )) )
       (dolist (pred preds)
         (push (asoc-filter-values pred nil)
               result))
       result)
     :result '(nil nil))
    ;; empty list, predicates with wrong number of arguments
    (should-equal
     (let ( result
            (preds '( >
                      (lambda (a b c d e) t) )) )
       (dolist (pred preds)
         (push (asoc-filter-values pred nil)
               result))
       result)
     :result '(nil nil))
    ;; constant true and false functions
    (should-equal
     (let ( table
            (preds  '(( truefn  . (lambda (v) t)  )
                      ( falsefn . (lambda (v) nil ) )))
            (alists '( nil
                       ((x . t))
                       ((x . t) (x . t))
                       ((x . t) (y . t) (z . t)) )) )
       (dolist (fnpair preds)
         (let ( result
                (fn      (cdr fnpair))
                (fn-name (car fnpair)) )
           (dolist (alist alists)
             (push (asoc-filter-values fn alist)
                   result))
           (push (list fn-name ::: (reverse result)) table)))
       (reverse table))
     :result
     '(( truefn  ::: ( ( )  ((x . t))  ((x . t) (x . t))  ((x . t) (y . t) (z . t)) ))
       ( falsefn ::: ( ( )  ( )        ( )                ( )                       )))
     )
    ;; sample functions and alists
    (should-equal
     (let ((alist '((1 . 1) (2 . 4) (3 . 3) (4 . 2) (5 . 1) (6 . 7))))
       (asoc-filter-values (lambda (k) (< k 4))
                           alist))
     :result '((1 . 1) (3 . 3) (4 . 2) (5 . 1)))
    (should-equal
     (let ((alist '((a . 1) (b . nil) (c . 3) (nil . 4) (nil . nil))))
       (asoc-filter-values #'identity alist))
     :result '((a . 1) (c . 3) (nil . 4)))
    )

  (ert-deftest test-asoc-unit-tests-asoc-remove ()
    "Unit tests for `asoc-remove'."
    ;; test against asoc-filter
    (let ( (preds   '( (lambda (k v) t)
                       (lambda (k v) nil)
                       eq
                       (lambda (k v) (and k v)) ))
           (alists '( nil
                      ((x . t))
                      ((x . t) (x . t))
                      ((x . t) (y . t) (z . t))
                      ((1 . 1) (2 . 4) (3 . 3) (4 . 2) (5 . 1) (6 . 7))
                      ((a . 1) (b . nil) (c . 3) (nil . 4) (nil . nil)) )) )
      (dolist (pred preds)
        (dolist (alist alists)
          (should-equal
           (asoc-remove pred alist)
           :result
           (asoc-filter (lambda (k v) (not (funcall pred k v)))
                        alist)))))
    ;; empty list, predicates with wrong number of arguments
    (should-equal
     (let ( result
            (preds-wrong-arity '( symbolp
                                  (lambda (a b c d e) t) )) )
       (dolist (pred preds-wrong-arity)
         (push (asoc-filter pred nil)
               result))
       result)
     :result '(nil nil))
    )

  (ert-deftest test-asoc-unit-tests-asoc-remove-keys ()
    "Unit tests for `asoc-remove-keys'."
    ;; test against asoc-filter-keys
    (let ( table
           (preds  '( (lambda (k) t)
                      (lambda (k) nil) ))
           (alists '( nil
                      ((x . t))
                      ((x . t) (x . t))
                      ((x . t) (y . t) (z . t)) )) )
      (dolist (pred preds)
        (dolist (alist alists)
          (should-equal
           (asoc-remove-keys pred alist)
           :result
           (asoc-filter-keys (lambda (k) (not (funcall pred k)))
                             alist)))))
    ;; empty list, predicates with wrong number of arguments
    (should-equal
     (let ( result
            (preds '( <
                      (lambda (a b c d e) t))) )
       (dolist (pred preds)
         (push (asoc-remove-keys pred nil)
               result))
       result)
     :result '(nil nil))
    ;; sample functions and alists
    (should-equal
     (let ((alist '((1 . 1) (2 . 4) (3 . 3) (4 . 2) (5 . 1) (6 . 7))))
       (asoc-remove-keys (lambda (k) (< k 4))
                         alist))
     :result '((4 . 2) (5 . 1) (6 . 7)))
    (should-equal
     (let ((alist '((a . 1) (b . nil) (c . 3) (nil . 4) (nil . nil))))
       (asoc-remove-keys #'identity alist))
     :result '((nil . 4) (nil)))
    )

  (ert-deftest test-asoc-unit-tests-asoc-remove-values ()
    "Unit tests for `asoc-remove-values'."
    ;; test against asoc-filter-values
    (let ( table
           (preds  '( (lambda (k) t)
                      (lambda (k) nil) ))
           (alists '( nil
                      ((x . t))
                      ((x . t) (x . t))
                      ((x . t) (y . t) (z . t)) )) )
      (dolist (pred preds)
        (dolist (alist alists)
          (should-equal
           (asoc-remove-values pred alist)
           :result
           (asoc-filter-values (lambda (k) (not (funcall pred k)))
                               alist)))))
    ;; empty list, predicates with wrong number of arguments
    (should-equal
     (let ( result
            (preds '( <
                      (lambda (a b c d e) t))) )
       (dolist (pred preds)
         (push (asoc-remove-values pred nil)
               result))
       result)
     :result '(nil nil))
    ;; sample functions and alists
    (should-equal
     (let ((alist '((1 . 1) (2 . 4) (3 . 3) (4 . 2) (5 . 1) (6 . 7))))
       (asoc-remove-values (lambda (k) (< k 4))
                           alist))
     :result '((2 . 4) (6 . 7)))
    (should-equal
     (let ((alist '((a . 1) (b . nil) (c . 3) (nil . 4) (nil . nil))))
       (asoc-remove-values #'identity alist))
     :result '((b) (nil)))
    )

  (ert-deftest test-asoc-unit-tests-asoc-partition ()
    "Unit tests for `asoc-partition'."
    ;; empty list
    (should-equal
     (asoc-partition nil)
     :result nil)
    ;; one element list: value is nil
    (should-equal
     (asoc-partition '(a))
     :result '((a . nil)))
    ;; two element list
    (should-equal
     (asoc-partition '(a 1))
     :result '((a . 1)))
    ;; three element list: last value is nil
    (should-equal
     (asoc-partition '(a 1 b 2 c))
     :result '((a . 1) (b . 2) (c . nil)))
    ;; repeats
    (should-equal
     (asoc-partition '(a 1 a 1))
     :result '((a . 1) (a . 1)))
    ;; non-list
    (should-error
     (asoc-partition 1)
     :type 'wrong-type-argument)
    (should-error
     (asoc-partition "string")
     :type 'wrong-type-argument)
    )

  (ert-deftest test-asoc-unit-tests-asoc-contains-key? ()
    "Unit tests for `asoc---contains-key?'."
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
             (push (asoc-contains-key? a test)
                   result))
           (push (reverse result) table)))
       table)
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
             (push (asoc-contains-pair? a (car test) (cdr test))
                   result))
           (push (reverse result) table)))
       table)
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
             (push (asoc-get a test)
                   result))
           (push (reverse result) table)))
       table)
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

  (ert-deftest test-asoc-unit-tests-asoc-delete! ()
    "Unit tests for `asoc-delete!'."
    ;; test nil, 1-pair (match/non-match), 2-pairs (match/non)
    (should-equal
     (let ((a nil))
       (asoc-delete! a 'x))
     :result nil)
    (should-equal
     (let ((a '((x 1))))
       (asoc-delete! a 'x))
     :result nil)
    (should-equal
     (let ((a '((y 2))))
       (asoc-delete! a 'x))
     :result '((y 2)))
    (should-equal
     (let ((a '((x 1) (y 2))))
       (asoc-delete! a 'x))
     :result '((y 2)))
    (should-equal
     (let ((a '((y 2) (x 1))))
       (asoc-delete! a 'x))
     :result '((y 2)))
    (should-equal
     (let ((a '((x 1) (x 11))))
       (asoc-delete! a 'x))
     :result '((x 11)))
    (should-equal
     (let ((a '((x 1) (x 11) (x 111))))
       (asoc-delete! a 'x))
     :result '((x 11) (x 111)))
    )

  (ert-deftest test-asoc-unit-tests-asoc-delete!/remove-all ()
    "Unit tests for `asoc-delete!' with remove-all set."
    ;; test nil, 1-pair (match/non-match), 2-pairs (match/non)
    (should-equal
     (let ((a nil))
       (asoc-delete! a 'x :all))
     :result nil)
    (should-equal
     (let ((a '((x 1))))
       (asoc-delete! a 'x :all))
     :result nil)
    (should-equal
     (let ((a '((y 2))))
       (asoc-delete! a 'x :all))
     :result '((y 2)))
    (should-equal
     (let ((a '((x 1) (y 2))))
       (asoc-delete! a 'x :all))
     :result '((y 2)))
    (should-equal
     (let ((a '((y 2) (x 1))))
       (asoc-delete! a 'x :all))
     :result '((y 2)))
    (should-equal
     (let ((a '((x 1) (x 11))))
       (asoc-delete! a 'x :all))
     :result nil)
    (should-equal
     (let ((a '((x 1) (x 11) (x 111))))
       (asoc-delete! a 'x :all))
     :result nil)
    (should-equal
     (let ((a '((x . 1) (y . 2) (x . 11) (z . 3) (x . 111) (x . 1111))))
       (asoc-delete! a 'x :all))
     :result '((y . 2) (z . 3)))
    (should-equal
     (let ((a '((x . 1) (y . 2) (x . 11) (z . 3) (x . 111) (x . 1111) (z . 33))))
       (asoc-delete! a 'x :all))
     :result '((y . 2) (z . 3) (z . 33)))
    )


  (ert-deftest test-asoc-unit-tests-asoc-find-key ()
    "Unit tests for `asoc-find-key'."
    (should-equal
     (let* (  table
              ( p  '(1 2) )
              ( a  `((1   . t) (2.0 . t) ("a" . t) (,p  . t) (nil . t)) )
               ;;            TEST-ITEM ;;    ALIST-ELEM
              (test-items `( 1         ;;    1           | 1  int
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
         (let ( (asoc-compare-fn  eqfn)
                (result  (list :: eqfn)) )
           (dolist (test test-items)
             (push (asoc-find-key test a)
                   result))
           (push (reverse result) table)))
       table)
     :result
     ;;  FN         1/1    1.0/1   2.0/2.0    2/2.0    "a"/"a"   "A"/"a"  (1 2)/(1 2) (1 2),same     nil
     '((eq     :: (1 . t)   nil      nil       nil       nil       nil        nil     ((1 2) . t) (nil . t))
       (eql    :: (1 . t)   nil   (2.0 . t)    nil       nil       nil        nil     ((1 2) . t) (nil . t))
       (equal  :: (1 . t)   nil   (2.0 . t)    nil    ("a" . t)    nil    ((1 2) . t) ((1 2) . t) (nil . t))
       (equalp :: (1 . t) (1 . t) (2.0 . t) (2.0 . t) ("a" . t) ("a" . t) ((1 2) . t) ((1 2) . t) (nil . t))))
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
           (push (list fn :: (asoc-keys a))
                 result)))
       result)
     :result   ;; <-first occurrences->         <-duplicates->
     '((eq     ::  (1  "a"  (1 2)  nil          1.0  "a" (1 2) ))
       (eql    ::  (1  "a"  (1 2)  nil          1.0  "a" (1 2) ))
       (equal  ::  (1  "a"  (1 2)  nil          1.0            ))
       (equalp ::  (1  "a"  (1 2)  nil                         )))
     )
    )

  (ert-deftest test-asoc-unit-tests-asoc-values ()
    "Unit tests for `asoc-values'."
    ;; empty alist
    (should-equal
     (let ((a nil))
       (asoc-values a))
     :result nil)
    ;; alist with duplicates
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (1 . 1) (3 . 9) (1 . 1) (4 . 16) (5 . 25))))
       (asoc-values a))
     :result '(1 4 9 16 25))
    ;; include shadowed values
    (should-equal
     (let ((a '((a . 1) (b . 2) (a . 3) (c . 4) (a . 5) (d . 6))))
       (asoc-values a))
     :result '(1 2 3 4 5 6))
    (should-equal
     (let ((a '((a . 1) (b . 2) (c . 3) (c . 4) (b . 5) (d . 6))))
       (asoc-values a))
     :result '(1 2 3 4 5 6))
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (1 . one) (3 . 9) (1 . "one") (4 . 16) (5 . 25))))
       (asoc-values a))
     :result '(1 4 one 9 "one" 16 25 ))
    ;; test with different choices of asoc-compare-fn
    (should-equal
     (let* ( result
            ( p  '(1 2) )
            ( a  `((1   . 1)   ("a" . "a") (,p    . ,p)    (nil . nil)
                   (1.0 . 1.0) ("a" . "a") ((1 2) . (1 2)) (nil . nil) )))

       (dolist (fn (list #'equalp #'equal #'eql #'eq))
         (let* (( asoc-compare-fn  fn  ))
           (push (list fn :: (asoc-values a))
                 result)))
       result)
     :result   ;; <-first occurrences->     <-duplicates->
     '((eq     ::  (1  "a"  (1 2)  nil      1.0  "a" (1 2) ))
       (eql    ::  (1  "a"  (1 2)  nil      1.0  "a" (1 2) ))
       (equal  ::  (1  "a"  (1 2)  nil      1.0            ))
       (equalp ::  (1  "a"  (1 2)  nil                     ))))
    )

  (ert-deftest test-asoc-unit-tests-asoc-values-ignore-values ()
    "Unit tests for `asoc-values'."
    ;; empty alist
    (should-equal
     (let ((a nil))
       (asoc-values a :ignore-shadowed))
     :result nil)
    ;; alist with duplicates
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (1 . 1) (3 . 9) (1 . 1) (4 . 16) (5 . 25))))
       (asoc-values a :ignore-shadowed))
     :result '(1 4 9 16 25))
    ;; exclude shadowed values
    (should-equal
     (let ((a '((a . 1) (b . 2) (a . 3) (c . 4) (a . 5) (d . 6))))
       (asoc-values a :ignore-shadowed))
     :result '(1 2 4 6))
    (should-equal
     (let ((a '((a . 1) (b . 2) (c . 3) (c . 4) (b . 5) (d . 6))))
       (asoc-values a :ignore-shadowed))
     :result '(1 2 3 6))
    ;; value shadowed by nil
    (should-equal
     (asoc-values '((a . nil) (a . 1))
                  :ignore-shadowed)
     :result '(nil))
    ;; test with different choices of asoc-compare-fn
    (should-equal
     (let* ( result
            ( p  '(1 2) )
            ( a  `((1   . 1)   ("a" . "a") (,p    . ,p)    (nil . nil)
                   (1.0 . 1.0) ("a" . "a") ((1 2) . (1 2)) (nil . nil) )))

       (dolist (fn (list #'equalp #'equal #'eql #'eq))
         (let* (( asoc-compare-fn  fn  ))
           (push (list fn :: (asoc-values a :ignore-shadowed))
                 result)))
       result)
     :result   ;; <-first occurrences->     <-duplicates->
     '((eq     ::  (1  "a"  (1 2)  nil      1.0  "a" (1 2) ))
       (eql    ::  (1  "a"  (1 2)  nil      1.0  "a" (1 2) ))
       (equal  ::  (1  "a"  (1 2)  nil      1.0            ))
       (equalp ::  (1  "a"  (1 2)  nil                     ))))
    )

  (ert-deftest test-asoc-unit-tests-asoc-unzip ()
    "Unit tests for `asoc-unzip'."
    ;; #keys == #values
    (should-equal
     (asoc-unzip '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))
     :result '((1 2 3 4 5) (1 4 9 16 25)))
    ;; #keys > #values
    (should-equal
     (asoc-unzip '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25) (6) (7)))
     :result '((1 2 3 4 5 6 7) (1 4 9 16 25 () ())))
    ;; empty list
    (should-equal
     (asoc-unzip nil)
     :result '(() ()))
    ;; empty values
    (should-equal
     (asoc-unzip '((1) (2) (3) (4) (5)))
     :result '((1 2 3 4 5) (() () () () ())))
    )

  (ert-deftest test-asoc-unit-tests-asoc-unzip-zip ()
    "Tests for `asoc-zip' reversing `asoc-unzip'."
    (let ((a '((1 . 1) (2 . 4) (1 . 1) (3 . 9) (1 . 1) (4 . 16) (5 . 25)))
          (b nil)
          (c '((1 . 1)))
          (d '((nil . nil))))
      (and
       (should-equal
        (apply #'asoc-zip (asoc-unzip a))
        :result a)
       (should-equal
        (apply #'asoc-zip (asoc-unzip b))
        :result b)
       (should-equal
        (apply #'asoc-zip (asoc-unzip c))
        :result c)
       (should-equal
        (apply #'asoc-zip (asoc-unzip d))
        :result d)
       ))
    )

  (ert-deftest test-asoc-unit-tests-asoc-zip-unzip ()
    "Tests for `asoc-unzip' reversing `asoc-zip'."
    ;; HOLDS
    (let ((a '((1 2 1 3 1 4 5) (1 4 1 9 1 16 25)))
          (b '(nil nil))
          (c '((1) (1)))
          (d '((nil) (nil))))
      (should-equal
       (asoc-unzip (apply #'asoc-zip a))
       :result a)
      (should-equal
       (asoc-unzip (apply #'asoc-zip b))
       :result b)
      (should-equal
       (asoc-unzip (apply #'asoc-zip c))
       :result c)
      (should-equal
       (asoc-unzip (apply #'asoc-zip d))
       :result d)
      )
    ;; DOESN'T HOLD
    (let (;; #keys > #values
          (a '((1 2 3 4 5 6 7) '(1 4 9 16 25)))
          ;; string argument
          (b '((1 2 3 4 5 6) "qwerty")))
      (should-not-equal
       (asoc-unzip (apply #'asoc-zip a))
       :result a)
      (should-not-equal
       (asoc-unzip (apply #'asoc-zip b))
       :result b))
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
    ;; return nil if result variable is not specified
    (should-equal
     (let ((a '((one . 1) (two . 4) (3 . 9) (4 . 16) (five . 25) (6 . 36))))
       (with-temp-buffer
         (asoc-do ((k v) a )
           (insert (format "%S\t%S\n" k v)))))
     :result nil)
    )

  (ert-deftest test-asoc-unit-tests-asoc--do ()
    "Unit tests for `asoc--do'."
    ;; null alist returns nil
    (should-equal
     (asoc--do nil
       (setq result 'result-set))
     :result nil)
    ;; null alist with :initially clause runs init code
    (should-equal
     (asoc--do nil
       (:initially (setq result 'result-set)))
     :result 'result-set)
    ;; empty body returns nil
    (should-equal
     (asoc--do nil)
     :result nil)
    (should-equal
     (asoc--do '((a . 1) (b . 2) (c . 4)))
     :result nil)
    ;; sample code and alists
    (should-equal
     (asoc--do '((a . 1) (b . 2) (c . 4))
       (push value result))
     :result '(4 2 1))
    (should-equal
     (asoc--do '((a . 1) (b . 2) (c . 4))
       (:initially (setq result 0))
       (setq result (+ result value)))
     :result 7)
    )

  (ert-deftest test-asoc-unit-tests-asoc-map ()
    "Unit tests for `asoc-map'."
    ;; empty list
    (should-equal
     (let ((a nil))
       (asoc-map (lambda (k v) (* k v)) a))
     :result nil)
    ;; #'cons is the identity
    (let ((alists '( nil
                     ((a . 1) (b . 2) (c . 3) (d . 4) (e . 5))
                     ((a . 1) (a . 2) (a . nil))
                     ((1 . 1) (2 . 2) (3 . 3) (4 . 4))         )))
      (dolist (a alists)
        (should-equal
         (asoc-map #'cons a)
         :result a)))
    ;; mapping #'list
    (should-equal
     (let ((a '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5))))
       (asoc-map #'list a))
     :result '((a 1) (b 2) (c 3) (d 4) (e 5)))
    )

  (ert-deftest test-asoc-unit-tests-asoc--map ()
    "Unit tests for `asoc--map'."
    ;; empty list
    (should-equal (asoc--map (* key value) nil) :result nil)
    ;; (cons key value) is the identity
    (let ((alists '( nil
                     ((a . 1) (b . 2) (c . 3) (d . 4) (e . 5))
                     ((a . 1) (a . 2) (a . nil))
                     ((1 . 1) (2 . 2) (3 . 3) (4 . 4))         )))
      (dolist (a alists)
        (should-equal
         (asoc--map (cons key value) a)
         :result a)))
    ;; mapping (list key value)
    (should-equal
     (let ((a '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5))))
       (asoc--map (list key value) a))
     :result '((a 1) (b 2) (c 3) (d 4) (e 5)))
    )

  (ert-deftest test-asoc-unit-tests-asoc-map-keys ()
    "Unit tests for `asoc-map-keys'."
    ;; empty list
    (should-equal
     (let ((a nil))
       (asoc-map-keys (lambda (k) (* k k)) a))
     :result nil)
    ;; #'identity
    (let ((alists '( nil
                     ((a . 1) (b . 2) (c . 3) (d . 4) (e . 5))
                     ((a . 1) (a . 2) (a . nil))
                     ((1 . 1) (2 . 2) (3 . 3) (4 . 4))         )))
      (dolist (a alists)
        (should-equal
         (asoc-map-keys #'identity a)
         :result a)))
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
    ;; #'identity
    (let ((alists '( nil
                     ((a . 1) (b . 2) (c . 3) (d . 4) (e . 5))
                     ((a . 1) (a . 2) (a . nil))
                     ((1 . 1) (2 . 2) (3 . 3) (4 . 4))         )))
      (dolist (a alists)
        (should-equal
         (asoc-map-values #'identity a)
         :result a)))
    )

  (ert-deftest test-asoc-unit-tests-asoc-fold ()
    "Unit tests for `asoc-fold'."
    ;; fold values only: sum of values
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25) (6 . 36))))
       (asoc-fold (lambda (k v acc) (+ acc v))
                  a 0))
     :result 91)
    ;; fold keys only: list of unique keys (reverse-encountered-order)
    (should-equal
     (let ((a '((1 . "one") (2 . "two") (1 . "eins") (2 . "zwei") (3 . "three"))))
       (asoc-fold (lambda (k v acc) (add-to-list 'acc k))
                  a nil))
     :result '(3 2 1))
    ;; fold keys and values: number of keys which equal their value
    (should-equal
     (let ((a '((1 . 1) (2 . 3) (3 . 2) (4 . 4) (5 . 8) (6 . 5) (7 . 7))))
       (asoc-fold (lambda (k v acc) (if (eq k v) (1+ acc) acc))
                  a 0))
     :result 3)
    ;; ... build string
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))
           (s ""))
       (asoc-fold (lambda (k v acc)
                    (concat acc (format "%S\t%S\n" k v)))
                  a ""))
     :result "1\t1\n2\t4\n3\t9\n4\t16\n5\t25\n")
    )

  (ert-deftest test-asoc-unit-tests-asoc--fold ()
    "Unit tests for `asoc--fold'."
    ;; fold values only: sum of values
    (should-equal
     (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25) (6 . 36))))
       (asoc-fold (lambda (k v acc) (+ acc v))
                  a 0))
     :result 91)
    ;; fold keys only: list of unique keys (reverse-encountered-order)
    (should-equal
     (let ((a '((1 . "one") (2 . "two") (1 . "eins") (2 . "zwei") (3 . "three"))))
       (asoc-fold (lambda (k v acc) (add-to-list 'acc k))
                  a nil))
     :result '(3 2 1))
    ;; fold keys and values: number of keys which equal their value
    (should-equal
     (let ((a '((1 . 1) (2 . 3) (3 . 2) (4 . 4) (5 . 8) (6 . 5) (7 . 7))))
       (asoc--fold (if (eq key value) (1+ acc) acc)
         a 0))
     :result 3)
    ;; ... number of elements with nil value
    (should-equal
     (let ((a '((1 . 1) (2 . nil) (3 . 2) (4 . nil) (5 . nil) (6 . 5) (7 . nil))))
       (asoc--fold (if (null value) (1+ acc) acc)
         a 0))
     :result 4)
    )

  (ert-deftest test-asoc-unit-tests-asoc-merge-values ()
    "Unit tests for `asoc-merge-values'."
    ;; empty list(s)
    (should-equal
     (asoc-merge-values nil)
     :result nil)
    (should-equal
     (asoc-merge-values nil nil nil)
     :result nil)
    ;; repeated key, one list
    (should-equal
     (asoc-merge-values '((a . 1) (a . 2) (a . 1)))
     :result '((a 1 2 1)))
    ;; no repeated keys, one list
    (should-equal
     (asoc-merge-values '((a . 1) (b . 2) (c . 3)))
     :result '((a 1) (b 2) (c 3)))
    ;; repeated key, multiple lists
    (should-equal
     (let ((al1 '((a . 1) (a . 2) (a . 3)))
           (al2 '((a . 1) (a . 2)))
           (al3 '((a . 4) (a . 5) (a . 6))))
     (asoc-merge-values al1 al2 al3))
     :result '((a 1 2 3 1 2 4 5 6)))
    ;; repeated key with null values
    (should-equal
     (asoc-merge-values '((a . nil) (a . nil) (a . nil)))
     :result '((a nil nil nil)))
    ;; multiple lists, multiple distinct repeated keys
    (should-equal
     (let ((al1 '((a . 1) (b . 2) (a . 3)))
           (al2 '((b . 1) (b . 2) (c . 3)))
           (al3 '((b . 4) (d . 5) (b . 6))))
       (asoc-merge-values al1 al2 al3))
     :result '((a 1 3) (b 2 1 2 4 6) (c 3) (d 5)))
    ;; no repeated keys, multiple lists
    (should-equal
     (let ((al1 '((a . 1) (b . 2) (c . 3)))
           (al2 '((c . 4) (d . 5)))
           (al3 '((e . 6) (f . 7))))
       (asoc-merge-values al1 al2 al3))
     :result '((a 1) (b 2) (c 3 4) (d 5) (e 6) (f 7)))
    ;; non-destructive
    (should-equal
     (let ((al1 '((a . 1) (b . 2) (a . 3)))
           (al2 '((b . 1) (b . 2) (c . 3)))
           (al3 '((b . 4) (d . 5) (b . 6))))
       (asoc-merge-values al1 al2 al3)
       (list al1 al2 al3))
     :result '( ((a . 1) (b . 2) (a . 3))
                ((b . 1) (b . 2) (c . 3))
                ((b . 4) (d . 5) (b . 6)) ))
    )

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
     (let ( (a '((a . 1) (b . 2) (a . 1) (c . 3) (b . 4)))
            (b '((a . 5))) )
       (asoc-merge-values a b))
     :result '((a 1 1 5) (b 2 4) (c 3))))

  )

(defun asoc---test-all ()
  (interactive)
  (ert-run-tests-batch "^test-asoc" ))
