Asoc.el
---------

Associative list (alist) library for Emacs Lisp.

##API

### variables
* asoc-compare-fn

### Constructor Functions
* asoc-make `(&optional keys)`

### Predicates
* asoc-contains-key? `(alist key)`
* asoc-contains-pair? `(alist key value)`

### Access Functions
* asoc-get `(asoc-get key alist &optional default remove)`
* asoc-put! `(key value alist &optional replace)`
* asoc-find-key `(asoc-find-key key list)`

### Looping Constructs
* asoc-do `(spec &rest body)`

### Mapping Functions
* asoc-map-values `(func alist)`
* asoc-zip `(keys values)`

### Folds 
* asoc-fold `(alist init function)`
