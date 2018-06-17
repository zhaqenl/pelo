#-asdf3.1 (error "ASDF 3.1 or bust!")

(defsystem "pelo"
    :version "0.0.1"
    :description "Common Lisp ping"
    :license "MIT"
    :author "Raymund Martinez"
    :class :package-inferred-system
    :depends-on ((:version "cl-scripting" "0.1")
                 (:version "inferior-shell" "2.0.3.3")
                 (:version "fare-utils" "1.0.0.5")
                 #:net.didierverna.clon
                 "pelo/pelo"))
