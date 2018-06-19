;;;; pelo.lisp

(uiop:define-package #:pelo/pelo
    (:use #:cl
          #:cl-scripting
          #:fare-utils
          #:net.didierverna.clon)
  (:export #:pelo))

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

(defvar *interval* 1 "Time in between pings")
(defvar *count* 1 "Number of packets to send (initialized to 1 as placeholder)")

(defvar *sent* 0 "Number of packets sent")
(defvar *received* 0 "Number of packets received")

(defvar *beep-online* nil "For the beep on check")
(defvar *beep-offline* nil "For the beep off check")

(defvar *online* "common-lisp/pelo/resources/online.mp3" "Sound file for online host")
(defvar *offline* "common-lisp/pelo/resources/offline.mp3" "Sound file for offline host")

(defun home (path)
  "Return pathname with merged home directory."
  (uiop:merge-pathnames* path (user-homedir-pathname)))

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

(defun beep-on-p ()
  "Check if beep online flag is provided."
  (and (host-present-p) (get-opt "b")))

(defun beep-off-p ()
  "Check if beep online flag is provided."
  (and (host-present-p) (get-opt "B")))

(defun interval-p ()
  "Check if host is provided and interval option is present."
  (and (host-present-p) (get-opt "i")))

(defun count-p ()
  "Check if host is provided with the count option."
  (and (host-present-p) (get-opt "c")))

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

(defun received-up ()
  "Increment *received* variable by 1."
  (incf *received*))

(defun change-count ()
  "Set the value of *count* to value of -c option. "
  (setf *count* (get-opt "c")))

(defun change-interval ()
  "Set the value of *interval* to value of -i option."
  (setf *interval* (get-opt "i")))

(defun enable-beep-on ()
  "Set value of *beep-online* to t."
  (setf *beep-online* t))

(defun enable-beep-off ()
  "Set value of *beep-offline* to nil."
  (setf *beep-offline* t))

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
  (format t "~&~A~&" date)
  (sleep *interval*))

(defun alive-print (date ping)
  "Print date with the ping."
  (format t "~&~A ~A~&" date ping)
  (sleep *interval*))

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
  (cond (count-p (count-down)
                 (and *beep-online* (beep-on))
                 (alive-print date ping)
                 (ping-host host count-p))
        (t (and *beep-online* (beep-on))
           (alive-print date ping)
           (ping-host host count-p))))

(defun ping-host (host count-p)
  "Send ping to host continually."
  (let ((output (get-ping host))
        (date (get-date)))
    (cond ((= 0 *count*) (statistics))
          ((string= output "") (sent-up) (dead-ping date host count-p))
          (t (sent-up) (received-up) (alive-ping date host output count-p)))))

(defun main (host)
  "The entry point"
  (declare (ignorable host))
  (handler-case
      (cond ((and (count-p) (interval-p) (beep-on-p)) ; check for -c -i -b
             (enable-beep-on)
             (change-count)
             (change-interval)
             (ping-host (remainder) t))
            ((and (count-p) (interval-p) (beep-off-p)) ; check for -c -i -B
             (enable-beep-off)
             (change-count)
             (change-interval)
             (ping-host (remainder) t))
            ((and (count-p) (interval-p)) ; check for -c -i
             (change-count)
             (change-interval)
             (ping-host (remainder) t))
            ((and (count-p) (beep-on-p)) ; check for -c -b
             (change-count)
             (enable-beep-on)
             (ping-host (remainder) t))
            ((and (count-p) (beep-off-p)) ; check for -c -B
             (change-count)
             (enable-beep-off)
             (ping-host (remainder) t))
            ((and (interval-p) (beep-on-p)) ; check for -i -b
             (change-interval)
             (enable-beep-on)
             (ping-host (remainder) nil))
            ((and (interval-p) (beep-off-p)) ; check for -i -B
             (change-interval)
             (enable-beep-off)
             (ping-host (remainder) nil))
            ((beep-on-p) ; check for -b
             (enable-beep-on)
             (ping-host (remainder) nil))
            ((beep-off-p) ; check for -B
             (enable-beep-off)
             (ping-host (remainder) nil))
            ((count-p) ; check for -c
             (change-count)
             (ping-host (remainder) t))
            ((interval-p) ; check for -i
             (change-interval)
             (ping-host (remainder) nil))
            ((host-present-p)
             (ping-host (remainder) nil))
            ((or (help-p) t)
             (print-help)))
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
