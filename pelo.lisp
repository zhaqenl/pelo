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
                  :typespec 'fixnum :argument-name "SECONDS"
                  :description "Ping interval.")
         (lispobj :short-name "c" :long-name "count"
                  :typespec 'fixnum :argument-name "NUMBER"
                  :description "Number of packets to send.")))

(defvar *interval* 1 "Time in between pings")
(defvar *count* 1 "Number of packets to send (initialized to 1 as placeholder)")
(defvar *sent* 0 "Number of packets sent")
(defvar *received* 0 "Number of packets received")

(defun count-down ()
  "Decrement *count* variable by 1."
  (decf *count*))

(defun sent-up ()
  "Increment *sent* variable by 1."
  (incf *sent*))

(defun received-up ()
  "Increment *received* variable by 1."
  (incf *received*))

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

(defun count-p ()
  "Check if host is provided with the count option."
  (and (host-present-p) (get-opt "c")))

(defun get-date ()
  "Get current formatted date as string."
  (inferior-shell:run/ss `(date "+%Y-%m-%d %H:%M:%S")))

(defun statistics ()
  "Show stats of the pelo runtime."
  (format *debug-io* "~a" (format nil "~&~%Sent: ~A ~&Received: ~A ~&Percent loss: ~A%~&" *sent*
                                  *received* (cond ((= 0 *received*) 100)
                                                   ((= 1 (/ *received* *sent*)) 0)
                                                   (t (/ *received* (/ *sent* 1.0))))))
  (force-output *debug-io*)
  (exit))

(defun get-ping (host)
  "Get ping reply from host."
  (inferior-shell:run/ss `(inferior-shell:pipe (ping -c 1 ,host) (grep "time=")
                                               (sed -e "s/^.*time=//;s/ *ms$//"))))

(defun dead-print (date)
  "Print only the date."
  (format *debug-io* "~&~A~&" date)
  (force-output *debug-io*)
  (sleep *interval*))

(defun alive-print (date ping)
  "Print date with the ping."
  (format *debug-io* "~&~A ~A~&" date ping)
  (force-output *debug-io*)
  (sleep *interval*))

(defun dead-ping (date host count-p)
  "Increment the variables for (statistics), then output only the date if host is not alive."
  (if count-p
      (progn (count-down)
             (sent-up)
             (dead-print date)
             (ping-host host count-p))
      (progn (sent-up)
             (dead-print date)
             (ping-host host count-p))))

(defun alive-ping (date host ping count-p)
  "Increment the variables for (statistics), then output the date and ping."
  (if count-p
      (progn (count-down)
             (sent-up)
             (received-up)
             (alive-print date ping)
             (ping-host host count-p))
      (progn (sent-up)
             (received-up)
             (alive-print date ping)
             (ping-host host count-p))))

(defun ping-host (host count-p)
  "Send ping to host continually."
  (let ((output (get-ping host))
        (date (get-date)))
    (cond ((= 0 *count*) (statistics))
          ((string= output "") (dead-ping date host count-p))
          (t (alive-ping date host output count-p)))))

(defun main (host)
  "The entry point"
  (declare (ignorable host))
  (handler-case
      (cond ((and (count-p) (interval-p)) (setf *count* (get-opt "c"))
             (setf *interval* (get-opt "i"))
             (ping-host (remainder) t))
            ((count-p) (setf *count* (get-opt "c"))
             (ping-host (remainder) t))
            ((interval-p) (setf *interval* (get-opt "i"))
             (ping-host (remainder) nil))
            ((host-present-p) (ping-host (remainder) nil))
            ((or (help-p) t) (print-help)))
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl  ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     ()
      (statistics))))

(exporting-definitions
  (defun pelo ()
    "The name of the created script"))

(register-commands :pelo/pelo)
