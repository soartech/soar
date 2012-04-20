
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

(defclass environment () () (:documentation "The RL-Glue environment."))

(defgeneric env-init (env)
  (:documentation
   "This routine will be called exactly once for each trial/run. This 
function is an ideal place to initialize all environment information 
and allocate any resources required to represent the environment. It 
must return a task specification which adheres to the task specification 
language. A task specification stores information regarding the 
observation and action space, as well as whether the task is episodic 
or continuous.

PARAMETERS:
    env : environment object in use [environment]

RETURNS:
    task specification [string]"))

(defgeneric env-start (env)
  (:documentation
   "For a continuing task this is done once. For an episodic task, this is 
done at the beginning of each episode. Env_start assembles a 
first observation given the agent is in the start state. Note the start 
state cannot also be a terminal state.

PARAMETERS:
    env : environment object in use [environment]

RETURNS:
    1st observation of an episode [observation]"))

(defgeneric env-step (env action)
  (:documentation
   "Complete one step in the environment. Take the action passed in and 
determine what the reward and next state are for that transition.

PARAMETERS:
    env    : environment object in use [environment]
    action : action to be performed [action]

RETURNS:
    reward        : reward of the step [float]
    observation   : observation after the step [observation]
    terminal flag : shows whether the episode is ended [boolean]"))

(defgeneric env-cleanup (env)
  (:documentation
   "This can be used to release any allocated resources. It will be called 
once for every call to env-init.

PARAMETERS:
    env : environment object in use [environment]

RETURNS:
    (none)"))

(defgeneric env-message (env input-message)
  (:documentation
   "Similar to agent-message, this function allows for any message passing 
to the environment required by the experiment program. This may be used 
to modify the environment mid experiment. Any information that needs to 
passed in or out of the environment can be handled by this function.

PARAMETERS:  
    env           : environment object in use [environment]
    input-message : recieved message [string]

RETURNS:
    output message to send [string]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun on-env-init (env buffer)
  "Handles the buffer operations for env-init."
  (declare #.*optimize-settings*)
  (let ((task-spec (env-init env)))
    (buffer-clear buffer)
    (rl-write-task-spec task-spec buffer))
  env)

(defun on-env-start (env buffer)
  "Handles the buffer operations for env-start."
  (declare #.*optimize-settings*)
  (let ((observation (env-start env)))
    (buffer-clear buffer)
    (rl-write-observation observation buffer))
  env)

(defun on-env-step (env buffer)
  "Handles the buffer operations for env-step."
  (declare #.*optimize-settings*)
  (let ((action (rl-read-action buffer)))
    (multiple-value-bind (reward observation terminal)
        (env-step env action)
      (buffer-clear buffer)
      (rl-write-terminal terminal buffer)
      (rl-write-reward reward buffer)
      (rl-write-observation observation buffer)))
  env)

(defun on-env-cleanup (env buffer)
  "Handles the buffer operations for env-cleanup."
  (declare #.*optimize-settings*)
  (env-cleanup env)
  (buffer-clear buffer)
  env)

(defun on-env-message (env buffer)
  "Handles the buffer operations for env-message."
  (declare #.*optimize-settings*)
  (let ((input-msg (rl-read-message buffer)))
    (let ((output-msg (env-message env input-msg)))
      (buffer-clear buffer)
      (rl-write-message output-msg buffer)))
  env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-env-event-loop (env socket buffer)
  "Communication loop of an environment with the rl-glue server."
  (declare #.*optimize-settings*)
  (loop do
       (buffer-clear buffer)
       (let ((state (rl-recv-buffer socket buffer)))
         (declare (fixnum state))
         (cond
           ((= state +k-env-init+)
            (on-env-init env buffer))
           ((= state +k-env-start+)
            (on-env-start env buffer))
           ((= state +k-env-step+)
            (on-env-step env buffer))
           ((= state +k-env-cleanup+)
            (on-env-cleanup env buffer))
           ((= state +k-env-message+)
            (on-env-message env buffer))
           ((= state +k-rl-term+)
            (return))
           (t (assert nil (state) "Unknown environment state: ~D" state)))
         (rl-send-buffer socket buffer state)))
  env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-env (env
                &key
                (host +k-localhost+)
                (port +k-default-port+)
                (max-retry nil)
                (retry-timeout +k-retry-timeout+)
                (autoreconnect nil))
  "Connects the specified ENV to RL-Glue on HOST and PORT. If the 
attempt is refused, it is tried again MAX-RETRY times, waiting for 
RETRY-TIMEOUT second between them.

PARAMETERS:
    env           : environment object in use [environment]
    host          : host name or address [string]
                    (key parameter, default is +k-localhost+)
    port          : port number [0 <= integer <= 65535]
                    (key parameter, default is +k-default-port+)
    max-retry     : maximum number of connection trials [nil or 0 < integer]
                    (key parameter, default is nil)
    retry-timeout : duration in seconds waited between retries [0 <= integer]
                    (key parameter, default is +k-retry-timeout+)
    autoreconnect : reconnecting after a finished experiment [boolean]
                    (key parameter, default is nil)
RETURNS:
    (none)"
  (forced-format "RL-Glue Lisp Environment Codec Version ~a, Build ~a~%"
                 (get-codec-version) (get-vc-codec-version))
  (rl-runner env +k-environment-connection+ #'run-env-event-loop
             host port max-retry retry-timeout autoreconnect))

