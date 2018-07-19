;;;; pelo.lisp

(uiop:define-package #:pelo/pelo
    (:use #:cl
          #:cl-scripting
          #:fare-utils
          #:net.didierverna.clon)
  (:export #:get-ping
           #:pelo))

(in-package :pelo/pelo)

(defsynopsis (:postfix "HOST")
  (text :contents "Send ICMP ECHO_REQUEST packets to network hosts. Press C-c, while pelo is
running, to end pelo and show the accumulated stats.
")
  (group (:header "Options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help.")
         (lispobj :short-name "i" :long-name "interval"
                  :typespec 'fixnum :argument-name "SECONDS"
                  :description "Ping interval.")
         (lispobj :short-name "c" :long-name "count"
                  :typespec 'fixnum :argument-name "NUMBER"
                  :description "Number of packets to send.")
         (flag :short-name "b" :long-name "beep-online"
               :description "Beep if host is up.")
         (flag :short-name "B" :long-name "beep-offline"
               :description "Beep if host is down.")))

(defvar *interval* 1
  "Time in between pings")

(defvar *count* 1
  "Number of packets to send (initialized to 1 as placeholder)")

(defvar *sent* 0
  "Number of packets sent")

(defvar *received* 0
  "Number of packets received")

(defvar *beep-online* nil
  "Beep on check")

(defvar *beep-offline* nil
  "Beep off check")

(defvar *count-p* nil
  "To check whether to count down.")

(defvar *online*
  "common-lisp/pelo/resources/online.mp3"
  "Sound file for online host")

(defvar *offline*
  "common-lisp/pelo/resources/offline.mp3"
  "Sound file for offline host")

(defun home (path)
  "Return pathname with merged home directory."
  (uiop:merge-pathnames* path (user-homedir-pathname)))

(defun print-help ()
  "Print help text."
  (help) (exit))

(defun host-present ()
  "Check if host is provided."
  (remainder :context (make-context)))

(defun beep-on ()
  "Play sound if host is online."
  (uiop:launch-program `("play" ,(namestring (home *online*)))))

(defun beep-off ()
  "Play sound if host is offline."
  (uiop:launch-program `("play" ,(namestring (home *offline*)))))

(defun count-down ()
  "Decrement *count* variable by 1."
  (decf *count*))

(defun sent-up ()
  "Increment *sent* variable by 1."
  (incf *sent*))

(defun get-date ()
  "Get current formatted date as string."
  (inferior-shell:run/ss `(date "+%Y-%m-%d %H:%M:%S")))

(defun count-sleep ()
  "Check value of *count*, if 0, then display stats, else sleep."
  (if (= 0 *count*)
      (statistics)
      (sleep *interval*)))

(defun statistics ()
  "Show stats of the pelo runtime."
  (format *debug-io*
          "~a"
          (format nil "~&~%Sent: ~A ~&Received: ~A ~&Percent loss: ~A%~&" *sent*
                  *received* (cond ((= 0 *received*) 100)
                                   ((= 1 (/ *received* *sent*)) 0)
                                   (t (/ *received* (/ *sent* 1.0))))))
  (force-output *debug-io*)
  (exit))

(defun get-ping (host)
  "Get ping reply from host."
  (inferior-shell:run/ss
   `(inferior-shell:pipe (ping -c 1 ,host) (grep "time=")
                         (sed -e "s/^.*time=//;s/ *ms$//"))))

(defun dead-print (date)
  "Print only the date."
  (format t "~&~A~&" date)
  (count-sleep))

(defun alive-print (date ping)
  "Print date with the ping."
  (format t "~&~A ~A~&" date ping)
  (count-sleep))

(defun dead-ping (date host count-p)
  "Increment the variables for (statistics), then output only the date."
  (if count-p
      (progn (count-down)
             (and *beep-offline* (beep-off))
             (dead-print date)
             (ping-host host count-p))
      (progn (and *beep-offline* (beep-off))
             (dead-print date)
             (ping-host host count-p))))

(defun alive-ping (date host ping count-p)
  "Increment the variables for (statistics), then output the date and ping."
  (if count-p
      (progn (count-down)
             (and *beep-online* (beep-on))
             (alive-print date ping)
             (ping-host host count-p))
      (progn (and *beep-online* (beep-on))
             (alive-print date ping)
             (ping-host host count-p))))

(defun ping-host (host count-p)
  "Send ping to host continually."
  (let ((output (get-ping host))
        (date (get-date)))
    (if (string= output "")
        (progn (sent-up)
               (dead-ping date host count-p))
        (progn (sent-up)
               (incf *received*)
               (alive-ping date host output count-p)))))

(defun main (host)
  "The entry point"
  (declare (ignorable host))
  (make-context)
  (do-cmdline-options (option name value source)
    (cond ((or (string= name "b") (string= name "beep-online")) (setf *beep-online* t))
          ((or (string= name "B") (string= name "beep-offline")) (setf *beep-offline* t))
          ((or (string= name "c") (string= name "count")) (setf *count-p* t)
           (setf *count* value))
          ((or (string= name "i") (string= name "interval")) (setf *interval* value)))) 
  (handler-case
      (cond ((or (getopt :short-name "h" :context (make-context)) (null (host-present)))
             (print-help))
            (t (ping-host (host-present) *count-p*)))
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl  ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     ()
      (statistics))))

(exporting-definitions
  (defun pelo (&rest args)
    "Canonical entry point"
    (apply #'main args)))

(register-commands :pelo/pelo)
