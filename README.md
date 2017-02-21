Asoc.el
---------

Associative list (alist) library for Emacs Lisp.

##API

### variables

* asoc-compare-fn

### functions & macros

* asoc-make `(&optional keys)`
* asoc-get `(asoc-get key alist &optional default remove)`
* asoc-put! `(key value alist &optional replace)`
* asoc-find-key `(asoc-find-key key list)`
* asoc-contains-key? `(alist key)`
* asoc-contains-pair? `(alist key value)`
* asoc-do `(spec &rest body)`
* asoc-map-values `(func alist)`
* asoc-zip `(keys values)`
* asoc-fold `(alist init function)`
 
