Asoc.el
---------

Associative list (alist) library for Emacs Lisp.

##API

### Variables
* asoc-compare-fn

### Constructor Functions
* [asoc-make](#asoc-make-optional-keys-default) `(&optional keys default)`

### Predicates
* [asoc-contains-key?](#asoc-contains-key-alist-key) `(alist key)`
* [asoc-contains-pair?](#asoc-contains-pair-alist-key-value) `(alist key value)`

### Access Functions
* [asoc-get](#asoc-get-key-alist-optional-default) `(key alist &optional default)`
* [asoc-put!](#asoc-put-keyvaluealistoptionalreplace) `(key value alist &optional replace)`
* [asoc-find-key](#asoc-find-key-key-alist-optional-test) `(key alist &optional test)`

### Looping Constructs
* [asoc-do](#asoc-do-spec-rest-body) `(spec &rest body)`

### Mapping Functions
* [asoc-map-values](#asoc-map-values-func-alist) `(func alist)`
* [asoc-zip](#asoc-zip-keys-values) `(keys values)`

### Filter Functions
* [asoc-filter](#asoc-filter-predicate-alist) `(predicate alist)`

### Folds
* [asoc-fold](#asoc-fold-func-alist-init) `(func alist init)`

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

## Predicates

### asoc-contains-key\? `(alist key)`

Return t if `alist` contains an item with key `key`, nil otherwise.

### asoc-contains-pair\? `(alist key value)`

Return t if `alist` contains an item (`key` . `value`), nil otherwise.

## Access Functions

### asoc-get `(key alist &optional default)`

Return the value associated with `key` in `alist`, or `default` if missing.

### asoc-put! `(`key` `value` `alist` &optional `replace`)`

Associate `key` with `value` in `alist`.

When `key` already exists, if `replace` is non-nil, previous entries with that `key`
are removed. Otherwise, the pair is simply consed on the front of the `alist`.
In the latter case, this is equivalent to `acons`.

### asoc-find-key `(key alist &optional test)`

Alias for `asoc--assoc`.


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

## Mapping Functions

### asoc-map-values `(func alist)`

Return a modified copy of alist with values transformed by `func`.

### asoc-zip `(keys values)`

Return an alist associating `keys` with corresponding `values`.
If `keys` is longer than `values`, the excess `keys` have value nil.

## Filters

### asoc-filter `(predicate alist)`

Return a copy of `alist` with key-value pairs satisfying `predicate` removed.

`predicate` should take two arguments, KEY and VALUE.

Example: filter out pairs where KEY > VALUE
    (let ((fib `((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-filter #'> fib))
    ;; ((2 . 1) (3 . 2) (4 . 3))


## Folds

### asoc-fold `(func alist init)`

Reduce `alist` using `func` on the values, starting with value `init`.

`func` should take a key, a value and the accumulated result and return
an updated result.

Example:
   (let ((a `((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))
         (s ""))
     (asoc-fold (lambda (k v acc)
                  (concat acc (format "%S	%S\n" k v)))
                a ""))
   "1	1
   2	4
   3	9
   4	16
   5	25
   "
