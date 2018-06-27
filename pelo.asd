#-asdf3.1 (error "ASDF 3.1 or bust!")

(defpackage :pelo-system
  (:use #:cl #:asdf))

(in-package #:pelo-system)

(defsystem :pelo
  :name "pelo"
  :version "0.0.2"
  :description "Common Lisp ping"
  :license "MIT"
  :author "Raymund Martinez <zhaqenl@gmail.com>"
  :class :package-inferred-system
  :depends-on ((:version "cl-scripting" "0.1")
               (:version "inferior-shell" "2.0.3.3")
               (:version "fare-utils" "1.0.0.5")
               #:net.didierverna.clon
               "pelo/pelo"))
