pelo
====

pelo is a periodic host monitor that utilizes ping to check for
host availability.


Installation
------------

Install the dependencies using the following commands, on Debian and NixOS, respectively:

```bash
sudo apt-get install -y sbcl cl-launch make git
```

```bash
nix-env -i sbcl cl-launch gnumake git
```

Then install pelo:

```bash
mkdir -p ~/bin ~/common-lisp
git clone https://github.com/fare/asdf ~/common-lisp/asdf
git clone https://github.com/zhaqenl/pelo ~/common-lisp/pelo
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --noinform --eval "(mapc #'ql:quickload '(:inferior-shell :clon :cl-launch :fare-utils :cl-scripting))" --quit
make -C ~/common-lisp/pelo install
```


## Credits

The structure of this README is based on [baf](https://github.com/ebzzry/baf/) by
[ebzzry](https://github.com/ebzzry).
