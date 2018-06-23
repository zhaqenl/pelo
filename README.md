pelo
====


pelo is a periodic host monitor that utilizes ping to check for host availability.


Table of contents
-----------------

- [Quickstart](#quickstart)
- [Installation](#installation)
- [Docker](#docker)
- [Credits](#credits)


<a name="quickstart">Quickstart</a>
-----------------------------------

If you simply want to try _pelo_ without having to build all the dependencies, run:

    docker run --security-opt seccomp=unconfined ebzzry/pelo 8.8.8.8

If you’re satisfied, you may create a shell function:

    pelo () { docker run --security-opt seccomp=unconfined ebzzry/pelo $@; }

a shell alias:

    alias pelo="docker run --security-opt seccomp=unconfined ebzzry/pelo"

or even a script in `~/bin`:

    cat > ~/bin/pelo << MEH
    #!/usr/bin/env bash
    docker run --security-opt seccomp=unconfined ebzzry/pelo $@
    MEH
    chmod +x ~/bin/pelo


<a name="installation">Installation</a>
---------------------------------------

Install the dependencies using the following commands, on Debian and NixOS systems, respectively:

```bash
sudo apt-get install -y curl sbcl cl-launch make git iputils-ping sox
```

```bash
nix-env -i curl sbcl cl-launch gnumake git iputils sox
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

The executable program will then be available at:

    ~/bin/pelo


<a name="docker">Docker</a>
---------------------------

To build the Docker image, run the following command inside the repository directory:

    docker build -t ogag/pelo .

To test it out:

    docker run --security-opt seccomp=unconfined ogag/pelo --help


<a name="credits">Credits</a>
-----------------------------

This was inspired by [pell](https://github.com/ebzzry/pell) and the structure of this README was
based on [baf](https://github.com/ebzzry/baf).
