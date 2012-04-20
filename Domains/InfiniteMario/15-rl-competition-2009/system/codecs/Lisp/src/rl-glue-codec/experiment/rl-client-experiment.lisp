
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

(defclass experiment ()
  ((exp-socket
    :accessor exp-socket
    :initform nil
    :documentation "Socket for the experiment server.")
   (exp-buffer
    :accessor exp-buffer
    :initform (make-buffer)
    :documentation "Buffer used for network communication."))
  (:documentation "The RL-Glue experiment."))

(defun rl-init (exp
                &key
                (host +k-localhost+)
                (port +k-default-port+)
                (max-retry nil)
                (retry-timeout +k-retry-timeout+))
  "This initializes everything, passing the environment's task specification 
to the agent. This should be called at the beginning of every trial. 
The rl-init function first connects the EXP to RL-Glue on HOST and PORT. 
If the attempt is refused, it is tried again MAX-RETRY times, waiting for 
RETRY-TIMEOUT second between them.

PARAMETERS:
    exp           : experiment in use [experiment]
    host          : host name or address [string]
                    (key parameter, default is +k-localhost+)
    port          : port number [0 <= integer <= 65535]
                    (key parameter, default is +k-default-port+)
    max-retry     : maximum number of connection trials [nil or 0 < integer]
                    (key parameter, default is nil)
    retry-timeout : duration in seconds waited between retries [0 <= integer]
                    (key parameter, default is +k-retry-timeout+)
RETURNS:
    task specification [string]"
  (declare #.*optimize-settings*)
  (with-accessors ((socket exp-socket) (buffer exp-buffer)) exp
    (unless socket
      (forced-format "RL-Glue Lisp Experiment Codec Version ~a, Build ~a~%"
                     (get-codec-version) (get-vc-codec-version))
      (setf socket (rl-wait-for-connection host
                                           port
                                           max-retry
                                           retry-timeout))
      (buffer-clear buffer)
      (rl-send-buffer socket buffer +k-experiment-connection+))
    (buffer-clear buffer)
    (rl-send-buffer socket buffer +k-rl-init+)
    (buffer-clear buffer)
    (assert (= +k-rl-init+
               (the fixnum (rl-recv-buffer socket buffer))))
    (rl-read-task-spec buffer)))

(defun rl-start (exp)
  "Do the first step of a run or episode. The action is saved in 
the upcoming-action slot so that it can be used on the next step.

PARAMETERS:
    exp : experiment in use [experiment]

RETURNS:
    first observation [observation]
    first action [action]"
  (declare #.*optimize-settings*)
  (with-accessors ((socket exp-socket) (buffer exp-buffer)) exp
    (buffer-clear buffer)
    (rl-send-buffer socket buffer +k-rl-start+)
    (buffer-clear buffer)
    (assert (= +k-rl-start+
               (the fixnum (rl-recv-buffer socket buffer))))
    (let ((observation (rl-read-observation buffer))
          (action (rl-read-action buffer)))
      (values observation action))))

(defun rl-step (exp)
  "Take one step. rl-step uses the saved action and saves the returned 
action for the next step. The action returned from one call must be 
used in the next, so it is better to handle this implicitly so that 
the user doesn't have to keep track of the action. If the 
end-of-episode observation occurs, then no action is returned.

PARAMETERS:
    exp : experiment in use [experiment]

RETURNS:
    reward of the step [float]
    observation after the step [observation]
    terminal flag after the step [boolean]
    next action (nil on terminal state) [action]"
  (declare #.*optimize-settings*)
  (with-accessors ((socket exp-socket) (buffer exp-buffer)) exp
    (buffer-clear buffer)
    (rl-send-buffer socket buffer +k-rl-step+)
    (buffer-clear buffer)
    (assert (= +k-rl-step+
               (the fixnum (rl-recv-buffer socket buffer))))
    (let ((terminal (rl-read-terminal buffer))
          (reward (rl-read-reward buffer))
          (obs (rl-read-observation buffer))
          (action (rl-read-action buffer)))
      (values reward obs terminal action))))

(defun rl-cleanup (exp)
  "Provides an opportunity to reclaim resources allocated by rl-init.

PARAMETERS:
    exp : experiment in use [experiment]

RETURNS:
    used experiment [experiment]"
  (declare #.*optimize-settings*)
  (with-accessors ((socket exp-socket) (buffer exp-buffer)) exp
    (buffer-clear buffer)
    (rl-send-buffer socket buffer +k-rl-cleanup+)
    (buffer-clear buffer)
    (assert (= +k-rl-cleanup+
               (the fixnum (rl-recv-buffer socket buffer)))))
  exp)

(defun rl-close (exp)
  "Finishes the experiment by closing its network resources (socket).

PARAMETERS:
    exp : experiment in use [experiment]

RETURNS:
    used experiment [experiment]"
  (with-accessors ((socket exp-socket)) exp
    (when socket
      (rl-close-socket socket)
      (setf socket nil)))
  exp)

(defun rl-return (exp)
  "Return the cumulative total reward of the current or just completed episode. 
The collection of all the rewards received in an episode (the return) is 
done within rl-return however, any discounting of rewards must be done 
inside the environment or agent.

PARAMETERS:
    exp : experiment in use [experiment]

RETURNS:
    cummulated total reward [float]"
  (declare #.*optimize-settings*)
  (with-accessors ((socket exp-socket) (buffer exp-buffer)) exp
    (buffer-clear buffer)
    (rl-send-buffer socket buffer +k-rl-return+)
    (buffer-clear buffer)
    (assert (= +k-rl-return+
               (the fixnum (rl-recv-buffer socket buffer))))
    (rl-read-reward buffer)))

(defun rl-num-steps (exp)
  "Return the number of steps elapsed in the current or just completed episode.

PARAMETERS:
    exp : experiment in use [experiment]

RETURNS:
    number of steps [0 <= integer]"
  (declare #.*optimize-settings*)
  (with-accessors ((socket exp-socket) (buffer exp-buffer)) exp
    (buffer-clear buffer)
    (rl-send-buffer socket buffer +k-rl-numsteps+)
    (buffer-clear buffer)
    (assert (= +k-rl-numsteps+
               (the fixnum (rl-recv-buffer socket buffer))))
    (buffer-read-int buffer)))

(defun rl-num-episodes (exp)
  "Return the number of episodes finished after rl-init.

PARAMETERS:
    exp : experiment in use [experiment]

RETURNS:
    number of episodes [0 <= integer]"
  (declare #.*optimize-settings*)
  (with-accessors ((socket exp-socket) (buffer exp-buffer)) exp
    (buffer-clear buffer)
    (rl-send-buffer socket buffer +k-rl-numepisodes+)
    (buffer-clear buffer)
    (assert (= +k-rl-numepisodes+
               (the fixnum (rl-recv-buffer socket buffer))))
    (buffer-read-int buffer)))

(defun rl-episode (exp &optional (max-num-steps 0))
  "Do one episode until a termination observation occurs or until steps have 
elapsed, whichever comes first. As you might imagine, this is done by 
calling rl-start, then rl-step until the terminal observation occurs. If 
max-num-steps is set to 0, it is taken to be the case where there is no 
limitation on the number of steps taken and rl-episode will continue until 
a termination observation occurs. If no terminal observation is reached 
before max-num-steps is reached, the agent does not call agent-end, it 
simply stops.

PARAMETERS:
    exp           : experiment in use [experiment]
    max-num-steps : maximum number of steps [0 < integer]
                    [optional parameter, default is no limit]
RETURNS:
    terminal flag after the step [boolean]"
  (declare #.*optimize-settings*)
  (with-accessors ((socket exp-socket) (buffer exp-buffer)) exp
    (buffer-clear buffer)
    (buffer-write-int max-num-steps buffer)
    (rl-send-buffer socket buffer +k-rl-episode+)
    (buffer-clear buffer)
    (assert (= +k-rl-episode+
               (the fixnum (rl-recv-buffer socket buffer))))
    (rl-read-terminal buffer)))

(defun rl-agent-message (exp message)
  "This message passes the input string to the agent and returns the 
reply string given by the agent. See agent-message for more details.

PARAMETERS:
    exp     : experiment in use [experiment]
    message : message to send [string]

RETURNS:
    recieved message [string]"
  (declare #.*optimize-settings*)
  (with-accessors ((socket exp-socket) (buffer exp-buffer)) exp
    (buffer-clear buffer)
    (rl-write-message message buffer)
    (rl-send-buffer socket buffer +k-rl-agent-message+)
    (buffer-clear buffer)
    (assert (= +k-rl-agent-message+
               (the fixnum (rl-recv-buffer socket buffer))))
    (rl-read-message buffer)))

(defun rl-env-message (exp message)
  "This message passes the input string to the environment and returns the 
reply string given by the environment. See env-message for more details.

PARAMETERS:
    exp     : experiment in use [experiment]
    message : message to send [string]

RETURNS:
    recieved message [string]"
  (declare #.*optimize-settings*)
  (with-accessors ((socket exp-socket) (buffer exp-buffer)) exp
    (buffer-clear buffer)
    (rl-write-message message buffer)
    (rl-send-buffer socket buffer +k-rl-env-message+)
    (buffer-clear buffer)
    (assert (= +k-rl-env-message+
               (the fixnum (rl-recv-buffer socket buffer))))
    (rl-read-message buffer)))

