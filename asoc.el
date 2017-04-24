;;; asoc.el --- alist functions and macros   -*- lexical-binding: t -*-

;; Copyright (C) 2016 Troy Pracy

;; Author: Troy Pracy
;; Keywords: alist data-types
;; Version: 0.1.2

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

(defun asoc--compare (x y)
  "Compare X and Y using `asoc-compare-fn'."
  (funcall (or asoc-compare-fn #'equal) x y))

(defun asoc--assoc (key alist &optional test)
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

(defun asoc--member (key list)
  "Return non-nil if KEY is a member of LIST.

Similar to `member', `memq' and `memql', but the equality test to used is
determined by `asoc-compare-fn'."
  (cond ((null list) nil)
        ((funcall (or asoc-compare-fn #'equal) key (car list)) list)
        ((asoc--member key (cdr list)))))

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
    ('equalp (and (asoc--assoc key alist #'equalp) t))
    ('eql    (and (asoc--assoc key alist #'eql) t))
    ('eq     (and (assq  key alist) t))
    (t       (and (assoc key alist) t))))

(defun asoc-contains-pair? (alist key value)
  "Return t if ALIST contains an item (KEY . VALUE), nil otherwise."
  (let (result)
    (dolist (pair alist)
      (when (and (asoc--compare (car pair) key)
                 (asoc--compare (cdr pair) value))
        (setq result t)))
    result))

;; ,------------------,
;; | Access Functions |
;; '------------------'

(defun asoc-get (alist key &optional default)
  "Return the value associated with KEY in ALIST, or DEFAULT if missing."
  (or (cdr (asoc--assoc key alist asoc-compare-fn)) default))

(defmacro asoc-put! (alist key value &optional replace)
  "Associate KEY with VALUE in ALIST.

When KEY already exists, if REPLACE is non-nil, previous entries with that key
are removed. Otherwise, the pair is simply consed on the front of the alist.
In the latter case, this is equivalent to `acons'."
  `(progn
     (when ,replace
       (setq ,alist (map-filter (lambda (k _) (not (asoc--compare k ,key)))
                                ,alist)))
     (setq ,alist (cons (cons ,key ,value) ,alist))))

(defalias 'asoc-find-key 'asoc--assoc)

(defun asoc-remove! (alist key &optional remove-all)
  "Remove the foremost element with KEY and return the modified list.

The list is modified in place unless the result is nil. If REMOVE-ALL is
non-nil, remove all elements with KEY."
  (let* ((head (car alist))
         (tail (cdr alist))
         (head-key (car head)))
    (cond
     ;; empty list
     ((null alist)  nil)
     ;; single pair
     ((null  tail)  (if (asoc--compare key head-key) nil alist))
     ;; two or more pairs
     (t
      (if (asoc--compare key head-key)    ;; remove foremost pair
          (progn
            (setcar alist (car tail))
            (setcdr alist (cdr tail))
            (if remove-all
                ;; recurse to remove other matches
                (asoc-remove! alist key remove-all)
              ;; return after first removal
              alist))
        (progn                             ;; keep foremost pair
          (unless (asoc-remove! tail key remove-all)
            (setcdr alist nil))
          alist))))))

(defun asoc-keys (alist)
  "Return a list of unique keys in ALIST."
  (let (result)
    (dolist (pair alist)
      (unless (asoc--member (car pair) result)
        (setq result (cons (car pair) result))))
    (reverse result)))

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
      `(mapcar (lambda (,pairsym)
                 (let ((,kvar (car ,pairsym))
                       (,vvar (cdr ,pairsym)))
                   ,@body))
               ,alist))))

;; ,-------------------,
;; | Mapping Functions |
;; '-------------------'

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
  (seq-filter (lambda (pair) (funcall predicate (car pair) (cdr pair))) alist))

(defun asoc-filter-keys (predicate alist)
  "Return a copy of ALIST with keys failing PREDICATE removed.

Example: filter for pairs where KEY <= 3
    (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-filter-keys (lambda (k) (<= k 3)) fib))
;; ((1 . 1) (2 . 1) (3 . 2))"
  (seq-filter (lambda (pair) (funcall predicate (car pair))) alist))

(defun asoc-filter-values (predicate alist)
  "Return a copy of ALIST with pairs whose value fails PREDICATE removed.

Example: filter for pairs where VALUE <= 3
    (let ((fib '((1 . 1)  (2 . 1)  (3 . 2)  (4 . 3)  (5 . 5)  (6 . 8)  (7 . 13)  (8 . 21))))
      (asoc-filter-values (lambda (v) (<= v 3)) fib))
;; ((1 . 1) (2 . 1) (3 . 2) (4 . 3))"
  (seq-filter (lambda (pair) (funcall predicate (cdr pair))) alist))

(defun asoc-uniq (alist)
  "Return a copy of ALIST with duplicate keys removed.

The foremost occurrence of each key is retained.

Example:
    (asoc-uniq '((a 1) (c 6) (b 2) (c 3) (d 4)))
    ;; ((a 1) (c 6) (b 2) (d 4))"
  (let (result keys)
    (dolist (pair alist result)
      (let ((k (car pair)))
        (unless (asoc--member k keys)
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

Example:
    (let ((a '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))
          (s \"\"))
      (asoc-fold (lambda (k v acc)
                   (concat acc (format \"%S\t%S\\n\" k v)))
                 a \"\"))
    \"1	1
    2	4
    3	9
    4	16
    5	25
    \"
"
  (let ((result init))
    (asoc-do
     ((key value) alist)
     (setq result (funcall func key value result)))
    result))


(provide 'asoc)

;;; asoc ends here
