Asoc.el
---------

Associative list (alist) library for Emacs Lisp.

##API

### variables

asoc-compare-fn

### functions & macros

* asoc-get `(asoc-get key alist &optional default remove)`
* asoc-put `(key value alist &optional replace)`
* asoc-find-key `(asoc-find-key key list)`
* asoc-contains-key? `(alist key)`
* asoc-contains-pair? `(alist key value)`
