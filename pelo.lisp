;;;; pelo.lisp

(uiop:define-package #:pelo/pelo
    (:use #:cl
          #:cl-scripting
          #:fare-utils
          #:net.didierverna.clon)
  (:export #:pelo))

(in-package :pelo/pelo)

(defsynopsis (:postfix "HOST")
  (text :contents "Send ICMP ECHO_REQUEST packets to network hosts.")
  (group (:header "Options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help.")
         (lispobj :short-name "i" :long-name "interval"
                  :typespec 'fixnum :argument-name "INT"
                  :description "Ping interval.")))

(defvar *interval* 1)

(defun get-opt (option)
  "Get the value of OPTION from the context."
  (getopt :short-name option :context (make-context)))

(defun help-p ()
  "Check if the help flag is provided."
  (get-opt "h"))

(defun print-help ()
  "Print help text."
  (help) (exit))

(defun host-present-p ()
  "Check if host is provided."
  (remainder :context (make-context)))

(defun interval-p ()
  "Check if host is provided and interval option is present."
  (and (host-present-p) (get-opt "i")))

(defun get-date ()
  "Get current formatted date as string."
  (inferior-shell:run/ss `(date "+%Y-%m-%d %H:%M:%S")))

(defun get-ping (host)
  "Get ping reply from host."
  (inferior-shell:run/ss `(inferior-shell:pipe (ping -c 1 ,host) (grep "time=")
                                               (sed -e "s/^.*time=//;s/ *ms$//"))))

(defun dead-ping (date host)
  "Output only the date if host is not alive."
  (format *debug-io* "~&~A~&" date)
  (force-output *debug-io*)
  (sleep *interval*)
  (ping-host host))

(defun alive-ping (date host ping)
  "Output the date and the corresponding ping."
  (format *debug-io* "~&~A ~A~&" date ping)
  (force-output *debug-io*)
  (sleep *interval*)
  (ping-host host))

(defun ping-host (host)
  "Send ping to host continually."
  (let ((output (get-ping host))
        (date (get-date)))
    (cond ((string= output "") (dead-ping date host))
          (t (alive-ping date host output)))))

(defun main (host)
  "The entry point"
  (declare (ignorable host))
  (make-context)
  (cond ((interval-p) (setf *interval* (get-opt "i"))
         (ping-host (remainder)))
        ((host-present-p) (ping-host (remainder)))
        ((help-p) (print-help))
        (t (print-help))))

(exporting-definitions
  (defun pelo ()
    "The name of the created script"))

(register-commands :pelo/pelo)
