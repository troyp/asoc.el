Asoc.el
---------

Associative list (alist) library for Emacs Lisp.

## API

### Variables
* [asoc-compare-fn](#asoc-compare-fn-nil)

### Constructor Functions
* [asoc-make](#asoc-make-optional-keys-default) `(&optional keys default)`
* [asoc-merge](#asoc-merge-rest-alists) `(&rest alists)`

### Predicates
* [asoc-contains-key?](#asoc-contains-key-alist-key) `(alist key)`
* [asoc-contains-pair?](#asoc-contains-pair-alist-key-value) `(alist key value)`

### Access Functions
* [asoc-get](#asoc-get-alist-key-optional-default) `(alist key &optional default)`
* [asoc-put!](#asoc-put-alist-key-value-optional-replace) `(alist key value &optional replace)`
* [asoc-find-key](#asoc-find-key-key-alist) `(key alist)`
* [asoc-delete!](#asoc-delete-alist-key-optional-remove-all) `(alist key &optional remove-all)`
* [asoc-keys](#asoc-keys-alist) `(alist)`
* [asoc-values](#asoc-values-alist) `(alist)`
* [asoc-unzip](#asoc-unzip-alist) `(alist)`

### Looping Constructs
* [asoc-do](#asoc-do-spec-rest-body) `(spec &rest body)`
* [asoc--do](#asoc--do-alist-rest-body) `(alist &rest body)`

### Mapping Functions
* [asoc-map](#asoc-map-function-alist) `(function alist)`
* [asoc-map-keys](#asoc-map-keys-func-alist) `(func alist)`
* [asoc-map-values](#asoc-map-values-func-alist) `(func alist)`
* [asoc-zip](#asoc-zip-keys-values) `(keys values)`

### Filter Functions
* [asoc-filter](#asoc-filter-predicate-alist) `(predicate alist)`
* [asoc-filter-keys](#asoc-filter-keys-predicate-alist) `(predicate alist)`
* [asoc-filter-values](#asoc-filter-values-predicate-alist) `(predicate alist)`
* [asoc-remove](#asoc-remove-predicate-alist) `(predicate alist)`
* [asoc-remove-keys](#asoc-remove-keys-predicate-alist) `(predicate alist)`
* [asoc-remove-values](#asoc-remove-values-predicate-alist) `(predicate alist)`
* [asoc-uniq](#asoc-uniq-alist) `(alist)`

### Folds
* [asoc-fold](#asoc-fold-func-alist-init) `(func alist init)`
* [asoc--fold](#asoc--fold-form-alist-init) `(form alist init)`

-------------------------------------------------------------------------------

## Variables

### asoc-compare-fn `nil`

Special variable holding the equality predicate used in asoc functions.

May take the values `equalp`, `equal`, `eql`, `eq`. When unset, or set to any
other value, functions default to using `equal`.

This variable may be passed to asoc functions dynamically in a let binding.

## Constructor Functions

### asoc-make `(&optional keys default)`

Return an alist with `keys` each initialized to value nil.

### asoc-merge `(&rest alists)`

Return an alist with unique keys resulting from merging `alists`.

When identical keys occur in two alists, the latter takes precedence. When
identical keys occur within a single alist, the foremost takes precedence.

## Predicates

### asoc-contains-key\? `(alist key)`

Return t if `alist` contains an item with key `key`, nil otherwise.

### asoc-contains-pair\? `(alist key value)`

Return t if `alist` contains an item (`key` . `value`), nil otherwise.

## Access Functions

### asoc-get `(alist key &optional default)`

Return the value associated with `key` in `alist`, or `default` if missing.

### asoc-put! `(alist key value &optional replace)`

Associate `key` with `value` in `alist`.

When `key` already exists, if `replace` is non-nil, previous entries with that `key`
are removed. Otherwise, the pair is simply consed on the front of the `alist`.
In the latter case, this is equivalent to `acons`.

### asoc-delete! `(alist key &optional remove-all)`

Return a modified list excluding the first, or all, pair(s) with `key`.

If `remove-all` is non-nil, remove all elements with `key`.

This may destructively modify `alist`.

### asoc-find-key `(key alist)`

Return the first element of `alist` whose `car` matches `key`, or nil if none match.

### asoc-keys `(alist)`

Return a list of unique keys in `alist`.

### asoc-values `(alist)`

Return a list of unique values in `alist`.

### asoc-unzip `(alist)`

Return a list of all keys and a list of all values in `alist`.

Returns `(KEYLIST VALUELIST) where KEYLIST and VALUELIST contain all the keys
and values in `alist` in order, including repeats. The original `alist` can be
reconstructed with

    (asoc-zip KEYLIST VALUELIST).

asoc-unzip will also reverse `asoc-zip` as long as the original arguments of
`asoc-zip` were both lists and were of equal length.

## Looping Constructs

### asoc-do `((keyvar valuevar) alist [result] body...)`

Iterate through `alist`, executing `body` for each key-value pair.

For each iteration, `keyvar` is bound to the key and `valuevar` is bound to the value.

The return value is obtained by evaluating `result`.

    (asoc-do ((k v) a)
      (insert (format "%S	%S\n" k v)))
    ;; print keys and values

    (let ((sum 0))
      (asoc-do ((key value) a sum)
        (when (symbolp key)
          (setf sum (+ sum value))))))
    ;; add values associated with all keys that are symbols.

### asoc--do `(alist &rest body)`

Anaphoric variant of `asoc-do`.

Iterate through `alist`, executing `body` for each key-value pair. For each
iteration, the anaphoric variables `key and 'value are bound to they current
key and value. The macro returns the value of the anaphoric variable 'result,
which is initially nil.

Optionally, initialization code can be included prior to the main body using
the syntax (:initially INITCODE...).

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

## Mapping Functions

### asoc-map `(function alist)`

Apply `func` to each element of `alist` and return the resulting list.

`func` should be a function of two arguments `(key value)`.

    ;; map value to nil when key is not a symbol...
    (asoc-map (lambda (k v) (cons k (when (symbolp k) v)))
              `((one . 1) (two . 4) (3 . 9) (4 . 16) (five . 25) (6 . 36)))
    ;; ((one . 1) (two . 4) (3 . nil) (4 . nil) (five . 25) (6 . nil))

    ;; list of values for symbol keys (nil for other keys)
    (asoc-map (lambda (k v) (when (symbolp k) v))
              '((one . 1) (two . 4) (3 . 9) (4 . 16) (five . 25) (6 . 36)))
    ;; (1 4 nil nil 25 nil)

### asoc-map-keys `(func alist)`

Return a modified copy of `alist` with keys transformed by `func`.

    ;; convert symbolic keys to strings
    (asoc-map-keys #'symbol-name
                   '((one . 1) (two . 4) (three . 9) (four . 16) (five . 25)))
    ;; (("one" . 1) ("two" . 4) ("three" . 9) ("four" . 16) ("five" . 25))

### asoc-map-values `(func alist)`

Return a modified copy of alist with values transformed by `func`.

    ;; convert alist to nested list
    (let ((a `((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))))
      (asoc-map-values #'list a))
    ;; ((1 1) (2 4) (3 9) (4 16) (5 25))

### asoc-zip `(keys values)`

Return an alist associating `keys` with corresponding `values`.
If `keys` is longer than `values`, the excess `keys` have value nil.

## Filters

### asoc-filter `(predicate alist)`

Return a copy of `alist` with key-value pairs failing `predicate` removed.

`predicate` should take two arguments, KEY and VALUE.

    ;; filter for pairs where KEY > VALUE
    (let ((fib `((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-filter #'> fib))
    ;; ((2 . 1) (3 . 2) (4 . 3))

### asoc-filter-keys `(predicate alist)`

Return a copy of `alist` with keys failing `predicate` removed.

    ;; filter for pairs where KEY <= 3
    (let ((fib `((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-filter-keys (lambda (k) (<= k 3)) fib))
    ;; ((1 . 1) (2 . 1) (3 . 2))

### asoc-filter-values `(predicate alist)`

Return a copy of `alist` with pairs whose value fails `predicate` removed.

    ;; filter for pairs where VALUE <= 3
    (let ((fib `((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-filter-values (lambda (v) (<= v 3)) fib))
    ;; ((1 . 1) (2 . 1) (3 . 2) (4 . 3))

### asoc-remove `(predicate alist)`

Return a copy of `alist` with key-value pairs satisfying `predicate` removed.

`predicate` should take two arguments, KEY and VALUE.

Alias: __`asoc-reject`__

    ;; filter out pairs where KEY > VALUE
    (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-remove #'> fib))
    ;; ((1 . 1) (5 . 5) (6 . 8) (7 . 13) (8 . 21))

### asoc-remove-keys `(predicate alist)`

Return a copy of `alist` with keys satisfying `predicate` removed.

Alias: __`asoc-reject-keys`__

    ;; filter out pairs where KEY <= 3
    (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-remove-keys (lambda (k) (<= k 3)) fib))
    ;; ((4 . 3) (5 . 5) (6 . 8) (7 . 13) (8 . 21))

### asoc-remove-values `(predicate alist)`

Return a copy of `alist` with pairs whose value satisfying `predicate` removed.

Alias: __`asoc-reject-values`__

    ;; filter out pairs where VALUE <= 3
    (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-remove-values (lambda (v) (<= v 3)) fib))
    ;; ((5 . 5) (6 . 8) (7 . 13) (8 . 21))

### asoc-uniq `(alist)`

Return a copy of `alist` with duplicate keys removed.

The foremost occurrence of each key is retained.

    (asoc-uniq `((a 1) (c 6) (b 2) (c 3) (d 4)))
    ;; ((a 1) (c 6) (b 2) (d 4))

## Folds

### asoc-fold `(func alist init)`

Reduce `alist` using `func` on the values, starting with value `init`.

`func` should take a key, a value and the accumulated result and return
an updated result.

Example: list of keys with value of 0

    (let ((a `((1 . 0) (2 . 0) (3 . 0) (4 . 1) (5 . 0)
               (6 . 2) (7 . 7) (8 . 3) (9 . 2) (10 . 0))))
      (asoc-fold (lambda (k v acc) (if (zerop v) (cons k acc) acc))
                 (reverse a) nil))
    ;; (1 2 3 5 10)

### asoc--fold `(form alist init)`

Anaphoric variant of `asoc-fold`.

  Reduce `alist` using `form` on each value, starting from `init`.

The anaphoric variables `key, 'value and 'acc represent the current
key, value and accumulated value, respectively.

The return value is the value of 'acc after the last element has
been processed.

Example: list of keys with value of 0

    (let ((a '((1 . 0) (2 . 0) (3 . 0) (4 . 1) (5 . 0)
              (6 . 2) (7 . 7) (8 . 3) (9 . 2) (10 . 0))))
      (asoc--fold (if (zerop value) (cons key acc) acc)
        (reverse a) nil))
    ;; (1 2 3 5 10)
