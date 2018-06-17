pelo
====

pelo is a periodic host monitor that utilizes ping to check for
host availability.


System dependencies
-------------------

- sbcl
- cl-launch
- make

Use your system package manager to install the above.


Lisp dependencies
-----------------

- inferior-shell
- clon
- cl-launch
- fare-utils
- cl-scripting

You may install the above with:

```
$ sbcl --noinform --eval "(mapc #'ql:quickload '(:inferior-shell :clon :cl-launch :fare-utils :cl-scripting))" --quit
```


Building
--------

To install `pelo` to `~/bin`, run:

```
$ make install
```
