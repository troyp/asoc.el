;;; asoc.el --- alist functions and macros   -*- lexical-binding: t -*-

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Keywords: alist data-types
;; Version: 0.2.8

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


;; ,-----------,
;; | Variables |
;; '-----------'

(defvar asoc-compare-fn nil
  "Special variable holding the equality predicate used in asoc functions.

May take the values `equalp', `equal', `eql', `eq'. When unset, or set to any
other value, functions default to using `equal'.

This variable may be passed to asoc functions dynamically in a let binding.")

;; ,-------------------,
;; | Private Functions |
;; '-------------------'

(defun asoc---compare (x y)
  "Compare X and Y using `asoc-compare-fn'."
  (funcall (or asoc-compare-fn #'equal) x y))

(defun asoc---assoc (key alist &optional test)
  "Return the first element of ALIST whose `car' matches KEY, or nil if none match.

The optional argument TEST specifies the equality test to be used, and defaults
to `equal'. Possible values include `eq', `eql', `equal', `equalp'."
  (case test
    ('eq          (assq  key alist))
    ((equal nil)  (assoc key alist))
    (t (progn
         (while (and alist
                     (not (funcall test (caar alist) key)))
           (setf alist (cdr alist)))
         (car alist)))))

(defun asoc---list-member (key list)
  "Return non-nil if KEY is a member of LIST.

Similar to `member', `memq' and `memql', but the equality test to used is
determined by `asoc-compare-fn'."
  (cond ((null list) nil)
        ((funcall (or asoc-compare-fn #'equal) key (car list)) list)
        ((asoc---list-member key (cdr list)))))

(defun asoc---list-filter (pred list)
  (let ((DELMARKER (make-symbol "DEL")))
    (delq
     DELMARKER
     (mapcar (lambda (x) (if (funcall pred x) x DELMARKER))
             list))))

(defun asoc---list-remove (pred list)
      (let ((DELMARKER (make-symbol "DEL")))
        (delq
        DELMARKER
        (mapcar (lambda (x) (if (funcall pred x) DELMARKER x))
                list))))


;; ,-----------------------,
;; | Constructor Functions |
;; '-----------------------'

(defun asoc-make (&optional keys default)
  "Return an alist with KEYS each initialized to value nil."
  (asoc-zip keys (make-list (length keys) default)))

;; ,------------,
;; | Predicates |
;; '------------'

(defun asoc-contains-key? (alist key)
  "Return t if ALIST contains an item with key KEY, nil otherwise."
  (case asoc-compare-fn
    ('equalp (and (asoc---assoc key alist #'equalp) t))
    ('eql    (and (asoc---assoc key alist #'eql) t))
    ('eq     (and (assq  key alist) t))
    (t       (and (assoc key alist) t))))

(defun asoc-contains-pair? (alist key value)
  "Return t if ALIST contains an item (KEY . VALUE), nil otherwise."
  (let (result)
    (dolist (pair alist)
      (when (and (asoc---compare (car pair) key)
                 (asoc---compare (cdr pair) value))
        (setq result t)))
    result))

;; ,------------------,
;; | Access Functions |
;; '------------------'

(defun asoc-get (alist key &optional default)
  "Return the value associated with KEY in ALIST, or DEFAULT if missing."
  (or (cdr (asoc---assoc key alist asoc-compare-fn)) default))

(defmacro asoc-put! (alist key value &optional replace)
  "Associate KEY with VALUE in ALIST.

When KEY already exists, if REPLACE is non-nil, previous entries with that key
are removed. Otherwise, the pair is simply consed on the front of the alist.
In the latter case, this is equivalent to `acons'."
  `(progn
     (when ,replace
       (setq ,alist (map-filter (lambda (k _) (not (asoc---compare k ,key)))
                                ,alist)))
     (setq ,alist (cons (cons ,key ,value) ,alist))))

(defun asoc-find-key (key alist)
    "Return the first element of ALIST whose `car' matches KEY, or nil if none match."
    (asoc---assoc key alist asoc-compare-fn))

(defun asoc-delete! (alist key &optional remove-all)
  "Return a modified list excluding the first, or all, pair(s) with KEY.

If REMOVE-ALL is non-nil, remove all elements with KEY.

This may destructively modify ALIST."
  (if ;; empty list
      (null alist)  nil
    ;; nonempty
    (let* ((head (car alist))
           (tail (cdr alist))
           (head-key (car head)))
      (if (asoc---compare key head-key)
          ;; recurse to other matches if remove-all==t
          (if remove-all (asoc-delete! tail key t) tail)
        (setcdr alist (asoc-delete! tail key remove-all))
        alist))))

(defun asoc-keys (alist)
  "Return a list of unique keys in ALIST."
  (let (result)
    (dolist (pair alist)
      (unless (asoc---list-member (car pair) result)
        (setq result (cons (car pair) result))))
    (reverse result)))

(defun asoc-values (alist)
  "Return a list of unique values in ALIST."
  (let (result)
    (dolist (pair alist)
      (unless (asoc---list-member (cdr pair) result)
        (setq result (cons (cdr pair) result))))
    (reverse result)))

(defun asoc-unzip (alist)
  "Return a list of all keys and a list of all values in ALIST.

Returns '(KEYLIST VALUELIST) where KEYLIST and VALUELIST contain all the keys
and values in ALIST in order, including repeats. The original alist can be
reconstructed with

    (asoc-zip KEYLIST VALUELIST).

asoc-unzip will also reverse `asoc-zip' as long as the original arguments of
`asoc-zip' were both lists and were of equal length."
  (let (keylist valuelist)
    (dolist (pair (reverse alist))
      (setq keylist (cons (car pair) keylist))
      (setq valuelist (cons (cdr pair) valuelist)))
    (list keylist valuelist)))

;; ,--------------------,
;; | Looping Constructs |
;; '--------------------'

(defmacro asoc-do (spec &rest body)
  "Iterate through ALIST, executing BODY for each key-value pair.

For each iteration, KEYVAR is bound to the key and VALUEVAR is bound to the value.

The return value is obtained by evaluating RESULT.

Example:

  (asoc-do ((k v) a)
    (insert (format \"%S\t%S\\n\" k v)))
  ;; print keys and values

  (let ((sum 0))
    (asoc-do ((key value) a sum)
      (when (symbolp key)
        (setf sum (+ sum value))))))
  ;; add values associated with all keys that are symbols.

\(fn ((KEYVAR VALUEVAR) ALIST [RESULT]) BODY...)"
  (declare (debug (((sexp sexp) form) body))
           (indent 1))
  (let* ((vars    (car spec))
         (kvar    (car vars))
         (vvar    (cadr vars))
         (alist   (cadr spec))
         (result  (caddr spec))
         (pairsym (make-symbol "pair")))
    (if result
        `(progn
           (mapcar (lambda (pair)
                     (let ((,kvar (car pair))
                           (,vvar (cdr pair)))
                       ,@body))
                   ,alist)
           ,result)
      `(progn
         (mapcar (lambda (,pairsym)
                   (let ((,kvar (car ,pairsym))
                         (,vvar (cdr ,pairsym)))
                     ,@body))
                 ,alist)
         nil))))

(defmacro asoc--do (alist &rest body)
  "Anaphoric variant of `asoc-do'.

Iterate through ALIST, executing BODY for each key-value pair. For each
iteration, the anaphoric variables 'key and 'value are bound to they current
key and value. The macro returns the value of the anaphoric variable 'result,
which is initially nil.

Optionally, initialization code can be included prior to the main body using
the syntax (:initially INITCODE...).

Example:

    (let ((a '((one . 1) (two . 4) (3 . 9) (4 . 16) (five . 25) (6 . 36))))
      (asoc--do a
        (when (symbolp key)
          (setf result (+ (or result 0) value)))))
    ;; 30

    (let ((a '((one . 1) (two . 4) (3 . 9) (4 . 16) (five . 25) (6 . 36))))
      (asoc--do a
        (:initially (setf result 0))
        (when (symbolp key)
          (setf result (+ result value)))))
    ;; 30

\(fn (ALIST [(:initially INITCODE...)] BODY...)"
  (declare (debug (sexp body))
           (indent 1))
  (let ((first-sexp (car body)))
    (if (and (listp first-sexp)
             (eq (car first-sexp) :initially))
        ;; with :initially form
        (let ((init (cdr first-sexp))
              (body (cdr body)))
          `(let ( result )
             ,@init
             (asoc-do ((key value) ,alist result) ,@body)))
      ;; no :initially form
      `(let ( result )
         (asoc-do ((key value) ,alist result) ,@body)))))

;; ,-------------------,
;; | Mapping Functions |
;; '-------------------'

(defun asoc-map (func alist)
  "Apply FUNC to each element of ALIST and return the resulting list.

FUNCTION should be a function of two arguments (KEY VALUE).

Examples:

    ;; map value to nil when key is not a symbol...
    (asoc-map (lambda (k v) (cons k (when (symbolp k) v)))
              '((one . 1) (two . 4) (3 . 9) (4 . 16) (five . 25) (6 . 36)))
    ;; ((one . 1) (two . 4) (3 . nil) (4 . nil) (five . 25) (6 . nil))

    ;; list of values for symbol keys (nil for other keys)
    (asoc-map (lambda (k v) (when (symbolp k) v))
              '((one . 1) (two . 4) (3 . 9) (4 . 16) (five . 25) (6 . 36)))
    ;; (1 4 nil nil 25 nil)"
  (mapcar (lambda (k.v)
            (let ((key   (car k.v))
                  (value (cdr k.v)))
              (funcall func key value)))
          alist))

(defun asoc-map-keys (func alist)
  "Return a modified copy of alist with keys transformed by FUNC.

Example: convert symbolic keys to strings

    (asoc-map-keys #'symbol-name
                  '((one . 1) (two . 4) (three . 9) (four . 16) (five . 25)))
    ;; ((\"one\" . 1) (\"two\" . 4) (\"three\" . 9) (\"four\" . 16) (\"five\" . 25))"
  (mapcar (lambda (k.v)
            (let ((k (car k.v))
                  (v (cdr k.v)))
              (cons (funcall func k) v)))
          alist))

(defun asoc-map-values (func alist)
  "Return a modified copy of alist with values transformed by FUNC.

Example: convert alist to nested list

    (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))))
      (asoc-map-values #'list a))
    ;; ((1 1) (2 4) (3 9) (4 16) (5 25))"
  (mapcar (lambda (k.v)
            (let ((k (car k.v))
                  (v (cdr k.v)))
              (cons k (funcall func v))))
          alist))

(defun asoc-zip (keys values)
  "Return an alist associating KEYS with corresponding VALUES.
If KEYS is longer than VALUES, the excess keys have value nil."
  (when (> (length values) (length keys))
    (error "More keys than values."))
  (let* ((n (- (length keys)
               (length values)))
         (values (append values (make-list n nil))))
    (mapcar* #'cons keys values)))

;; ,------------------,
;; | Filter Functions |
;; '------------------'

(defun asoc-filter (predicate alist)
  "Return a copy of ALIST with key-value pairs failing PREDICATE removed.

PREDICATE should take two arguments, KEY and VALUE.

Example: filter for pairs where KEY > VALUE

    (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-filter #'> fib))
    ;; ((2 . 1) (3 . 2) (4 . 3))"
  (asoc---list-filter (lambda (pair) (funcall predicate (car pair) (cdr pair))) alist))

(defun asoc-filter-keys (predicate alist)
  "Return a copy of ALIST with keys failing PREDICATE removed.

Example: filter for pairs where KEY <= 3

    (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-filter-keys (lambda (k) (<= k 3)) fib))
;; ((1 . 1) (2 . 1) (3 . 2))"
  (asoc---list-filter (lambda (pair) (funcall predicate (car pair))) alist))

(defun asoc-filter-values (predicate alist)
  "Return a copy of ALIST with pairs whose value fails PREDICATE removed.

Example: filter for pairs where VALUE <= 3

    (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-filter-values (lambda (v) (<= v 3)) fib))
;; ((1 . 1) (2 . 1) (3 . 2) (4 . 3))"
  (asoc---list-filter (lambda (pair) (funcall predicate (cdr pair))) alist))

(defun asoc-remove (predicate alist)
  "Return a copy of ALIST with key-value pairs satisfying PREDICATE removed.

PREDICATE should take two arguments, KEY and VALUE.

Alias: `asoc-reject'

Example: filter out pairs where KEY > VALUE

    (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-remove #'> fib))
    ;; ((1 . 1) (5 . 5) (6 . 8) (7 . 13) (8 . 21))"
  (asoc---list-remove (lambda (pair) (funcall predicate (car pair) (cdr pair))) alist))

(defun asoc-remove-keys (predicate alist)
  "Return a copy of ALIST with keys satisfying PREDICATE removed.

Alias: `asoc-reject-keys'

Example: filter out pairs where KEY <= 3

    (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-remove-keys (lambda (k) (<= k 3)) fib))
    ;; ((4 . 3) (5 . 5) (6 . 8) (7 . 13) (8 . 21))"
  (asoc---list-remove (lambda (pair) (funcall predicate (car pair))) alist))

(defun asoc-remove-values (predicate alist)
  "Return a copy of ALIST with pairs whose value satisfying PREDICATE removed.

Alias: `asoc-reject-values'

Example: filter out pairs where VALUE <= 3

    (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-remove-values (lambda (v) (<= v 3)) fib))
    ;; ((5 . 5) (6 . 8) (7 . 13) (8 . 21))"
  (asoc---list-remove (lambda (pair) (funcall predicate (cdr pair))) alist))

(defalias 'asoc-reject 'asoc-remove)
(defalias 'asoc-reject-keys 'asoc-remove-keys)
(defalias 'asoc-reject-values 'asoc-remove-values)

(defun asoc-uniq (alist)
  "Return a copy of ALIST with duplicate keys removed.

The foremost occurrence of each key is retained.

Example:

    (asoc-uniq '((a 1) (c 6) (b 2) (c 3) (d 4)))
    ;; ((a 1) (c 6) (b 2) (d 4))"
  (let (result keys)
    (dolist (pair alist result)
      (let ((k (car pair)))
        (unless (asoc---list-member k keys)
          (setq result (cons pair result))
          (setq keys (cons k keys))
          )))
    (nreverse result)))

;; ,-------,
;; | Folds |
;; '-------'

(defun asoc-fold (func alist init)
  "Reduce ALIST using FUNC on the values, starting with value INIT.

FUNC should take a key, a value and the accumulated result and return
an updated result.

Example: list of keys with value of 0

    (let ((a '((1 . 0) (2 . 0) (3 . 0) (4 . 1) (5 . 0)
               (6 . 2) (7 . 7) (8 . 3) (9 . 2) (10 . 0))))
      (asoc-fold (lambda (k v acc) (if (zerop v) (cons k acc) acc))
                 (reverse a) nil))
    ;; (1 2 3 5 10)"
  (let ((result init))
    (asoc-do
        ((key value) alist)
      (setq result (funcall func key value result)))
    result))

(defmacro asoc--fold (form alist init)
  "Anaphoric variant of `asoc-fold'.

  Reduce ALIST using FORM on each value, starting from INIT.

The anaphoric variables 'key, 'value and 'acc represent the current
key, value and accumulated value, respectively.

The return value is the value of 'acc after the last element has
been processed.

Example: list of keys with value of 0

    (let ((a '((1 . 0) (2 . 0) (3 . 0) (4 . 1) (5 . 0)
              (6 . 2) (7 . 7) (8 . 3) (9 . 2) (10 . 0))))
      (asoc--fold (if (zerop value) (cons key acc) acc)
        (reverse a) nil))
    ;; (1 2 3 5 10)"
  (declare (debug (sexp sexp sexp))
           (indent 1))
  `(asoc-fold (lambda (key value acc) ,form)
              ,alist ,init))


(provide 'asoc)

;;; asoc ends here
