
;;; Copyright 2008 Gabor Balazs
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
;;;
;;; $Revision: 408 $
;;; $Date: 2008-12-23 13:06:08 -0500 (Tue, 23 Dec 2008) $

(in-package #:org.rl-community.rl-glue-codec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; RL-Glue needs to know what type of object is trying to connect.

(defconstant +k-experiment-connection+ 1
  "Network code of experiment connection establishment event.")
(defconstant +k-agent-connection+ 2
  "Network code of agent connection establishment event.")
(defconstant +k-environment-connection+ 3
  "Network code of environment connection establishment event.")

;;; The server starts by sending one of these values to the client 
;;; to let it know what type of event to respond to.

(defconstant +k-agent-init+ 4
  "Network code of agent initialization event.")
(defconstant +k-agent-start+ 5
  "Network code of agent start event.")
(defconstant +k-agent-step+ 6
  "Network code of agent step event.")
(defconstant +k-agent-end+ 7
  "Network code of agent episode end event.")
(defconstant +k-agent-cleanup+ 8
  "Network code of agent cleanup event.")
(defconstant +k-agent-freeze+ 9
  "Network code of agent freeze event (deprecated and ignored).")
(defconstant +k-agent-message+ 10
  "Network code of agent message sending event.")

(defconstant +k-env-init+ 11
  "Network code of environment initialization event.")
(defconstant +k-env-start+ 12
  "Network code of environment start event.")
(defconstant +k-env-step+ 13
  "Network code of environment step event.")
(defconstant +k-env-cleanup+ 14
  "Network code of environment cleanup event.")
(defconstant +k-env-load-state+ 15
  "Network code of environment state loading event
 (deprecated and ignored).")
(defconstant +k-env-load-random-seed+ 16
  "Network code of environment random seed loading event
 (deprecated and ignored).")
(defconstant +k-env-save-state+ 17
  "Network code of environment state saving event
 (deprecated and ignored).")
(defconstant +k-env-save-random-seed+ 18
  "Network code of environment random seed saving event
 (deprecated and ignored).")
(defconstant +k-env-message+ 19
  "Network code of environment message sending event.")

(defconstant +k-rl-init+ 20
  "Network code of experiment initialization event.")
(defconstant +k-rl-start+ 21
  "Network code of experiment start event.")
(defconstant +k-rl-step+ 22
  "Network code of experiment step event.")
(defconstant +k-rl-cleanup+ 23
  "Network code of experiment cleanup event.")
(defconstant +k-rl-return+ 24
  "Network code of experiment total reward query event.")
(defconstant +k-rl-numsteps+ 25
  "Network code of experiment number of steps query event.")
(defconstant +k-rl-numepisodes+ 26
  "Network code of experiment number of episodes query event.")
(defconstant +k-rl-episode+ 27
  "Network code of experiment episode running event.")
(defconstant +k-rl-load-state+ 28
  "Network code of experiment state loading event
 (deprecated and ignored).")
(defconstant +k-rl-load-random-seed+ 29
  "Network code of experiment random seed loading event
 (deprecated and ignored).")
(defconstant +k-rl-save-state+ 30
  "Network code of experiment state saving event
 (deprecated and ignored).")
(defconstant +k-rl-save-random-seed+ 31
  "Network code of experiment random seed saving event
 (deprecated and ignored).")
(defconstant +k-rl-freeze+ 32
  "Network code of experiment freeze event
 (deprecated and ignored).")
(defconstant +k-rl-agent-message+ 33
  "Network code of experiment message sending to agent event.")
(defconstant +k-rl-env-message+ 34
  "Network code of experiment message sending to environment event.")
(defconstant +k-rl-term+ 35
  "Network code of experiment termination event.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +k-localhost+ "127.0.0.1"
  "IP address of the local host.")
(defparameter +k-default-port+ 4096
  "Default port on which connection is tried to the rl-glue server.")
(defparameter +k-retry-timeout+ 10
  "Default timeout seconds between two connection attempts.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun forced-format (fmtstr &rest args)
  "Writes to the standard output without any delay."
  (apply #'format *standard-output* fmtstr args)
  (force-output *standard-output*))

(defun rl-wait-for-connection (host port max-retry retry-timeout)
  "Waiting for a connection to be established. Returns the obtained socket 
descriptor on success, or nil on failure."
  (forced-format "        Connecting to ~a:~a " host port)
  (loop with retry = 0 do
       (forced-format ".")
       (when max-retry
         (incf retry)
         (when (< max-retry retry)
           (forced-format " failed~%")
           (return nil)))
       (handler-case
           (let ((socket (usocket:socket-connect host port
                                                 :element-type 'byte-t)))
             (forced-format " ok~%")
             (return socket))
         (error (e)
           (declare (ignore e)) (sleep retry-timeout)))))

(defun rl-close-socket (socket)
  "Closes the given socket."
  (usocket:socket-close socket))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rl-send-buffer (socket buffer state)
  "Sends the BUFFER content with STATE identifier."
  (declare #.*optimize-settings*)
  (buffer-send buffer (usocket:socket-stream socket) state)
  buffer)

(defun rl-recv-buffer (socket buffer)
  "Receives the BUFFER content and a state identifier what is returned."
  (declare #.*optimize-settings*)
  (buffer-recv buffer (usocket:socket-stream socket)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rl-runner (obj conn-state event-loop
                  host port max-retry retry-timeout autoreconnect)
  "Connects and runs the loop of OBJ."
  (assert (or (not max-retry) (< 0 max-retry)))
  (assert (<= 0 retry-timeout))
  (let ((buffer (make-buffer)) (socket nil))
    (loop
      (unwind-protect
           (progn
             (setf socket (rl-wait-for-connection host
                                                  port
                                                  max-retry
                                                  retry-timeout))
             (when socket
               (buffer-clear buffer)
               (rl-send-buffer socket buffer conn-state)
               (funcall event-loop obj socket buffer)))
        (when socket (rl-close-socket socket)))
      (unless autoreconnect (return))))
  obj)

