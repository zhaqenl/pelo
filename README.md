pelo
====


pelo is a periodic host monitor that utilizes ping to check for host availability.


Installation
------------

Install the dependencies using the following commands, on Debian and NixOS systems, respectively:

```bash
sudo apt-get install -y curl sbcl cl-launch make git
```

```bash
nix-env -i curl sbcl cl-launch gnumake git
```

Then, install pelo:

```bash
mkdir -p ~/bin ~/common-lisp
git clone https://github.com/fare/asdf ~/common-lisp/asdf
git clone https://github.com/zhaqenl/pelo ~/common-lisp/pelo
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --noinform --load quicklisp.lisp --eval  '(quicklisp-quickstart:install)' --eval '(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file) (sb-ext:exit))'
sbcl --noinform --eval "(progn (mapc #'ql:quickload '(:inferior-shell :clon :cl-launch :fare-utils :cl-scripting)) (sb-ext:exit))"
make -C ~/common-lisp/pelo install
```


Credits
-------

This was inspired by [pell](https://github.com/ebzzry/pell) and the structure of this README was
based on [baf](https://github.com/ebzzry/baf).
