FROM phusion/baseimage
MAINTAINER Rommel Martinez <ebzzry@ebzzry.io>
RUN apt-get update -y
RUN apt-get install -y curl sbcl cl-launch make git iputils-ping sox
RUN mkdir -p ~/bin ~/common-lisp
RUN git clone https://github.com/fare/asdf ~/common-lisp/asdf
RUN git clone https://github.com/zhaqenl/pelo ~/common-lisp/pelo
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --noinform --load quicklisp.lisp --eval  '(quicklisp-quickstart:install)' --eval '(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file) (sb-ext:exit))'
RUN sbcl --noinform --eval "(progn (mapc #'ql:quickload '(:inferior-shell :clon :cl-launch :fare-utils :cl-scripting)) (sb-ext:exit))"
RUN make -C ~/common-lisp/pelo
ENTRYPOINT [ "/root/common-lisp/pelo/pelo" ]
