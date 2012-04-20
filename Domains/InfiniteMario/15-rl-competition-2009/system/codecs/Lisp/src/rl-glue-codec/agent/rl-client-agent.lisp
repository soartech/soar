
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

(defclass agent () () (:documentation "The RL-Glue agent."))

(defgeneric agent-init (agent task-spec)
  (:documentation
   "This function will be called first, even before agent-start. TASK-SPEC 
is a description of important experiment information, including but not 
exclusive to a description of the state and action space. The RL-Glue 
standard for writing TASK-SPEC strings is found here. In agent-init, 
information about the environment is extracted from TASK-SPEC and then 
used to set up any necessary resources (for example, initialize the 
value function to a prelearning state).

PARAMETERS:
    agent     : agent object in use [agent]
    task-spec : task specification [string]

RETURNS:
    (none)"))

(defgeneric agent-start (agent first-observation)
  (:documentation
   "Given the FIRST-OBSERVATION (the observation of the agent in the start 
state) the agent must then return the action it wishes to perform. 
This is called once if the task is continuing, else it happens at the 
beginning of each episode.

PARAMETERS:
    agent             : agent object in use [agent]
    first-observation : 1st observation of an episode [observation]

RETURNS:
    1st action of the episode selected by the agent [action]"))

(defgeneric agent-step (agent reward observation)
  (:documentation
   "This is the most important function of the agent. Given the REWARD 
garnered by the agent's previous action, and the resulting OBSERVATION, 
choose the next action to take. Any learning (policy improvement) 
should be done through this function.

PARAMETERS:
    agent       : agent object in use [agent]
    reward      : obtained reward [float]
    observation : resulting observation [observation]

RETURNS:
    next action selected by the agent [action]"))

(defgeneric agent-end (agent reward)
  (:documentation
   "If the agent is in an episodic environment, this function will be 
called after the terminal state is entered. This allows for any final 
learning updates. If the episode is terminated prematurely (ie a 
benchmark cutoff before entering a terminal state) agent-end is NOT 
called.

PARAMETERS:
    agent  : agent object in use [agent]
    reward : last reward of an episode [float]

RETURNS:
    (none)"))

(defgeneric agent-cleanup (agent)
  (:documentation
   "This function is called at the end of a run/trial and can be used to 
free any resources which may have allocated in agent-init. Calls to 
agent-cleanup should be in a one to one ratio with the calls to 
agent-init.

PARAMETERS:
    agent : agent object in use [agent]

RETURNS:
    (none)"))

(defgeneric agent-message (agent input-message)
  (:documentation
   "The agent-message function is a jack of all trades and master of none. 
Having no particular functionality, it is up to the user to determine 
what agent-message should implement. If there is any information which 
needs to be passed in or out of the agent, this message should do it. 
For example, if it is desirable that an agent's learning parameters be 
tweaked mid experiment, the author could establish an input string that 
triggers this action. Likewise, if the author wished to extract a 
representation of the value function, they could establish an input 
string which would cause agent-message to return the desired 
information.

PARAMETERS:
    agent         : agent object in use [agent]
    input-message : recieved message [string]

RETURNS:
    output message to send [string]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun on-agent-init (agent buffer)
  "Handles the buffer operations for agent-init."
  (declare #.*optimize-settings*)
  (let ((task-spec (rl-read-task-spec buffer)))
    (agent-init agent task-spec))
  (buffer-clear buffer)
  agent)

(defun on-agent-start (agent buffer)
  "Handles the buffer operations for agent-start."
  (declare #.*optimize-settings*)
  (let ((observation (rl-read-observation buffer)))
    (let ((action (agent-start agent observation)))
      (buffer-clear buffer)
      (rl-write-action action buffer)))
  agent)

(defun on-agent-step (agent buffer)
  "Handles the buffer operations for agent-step."
  (declare #.*optimize-settings*)
  (let ((reward (rl-read-reward buffer))
        (observation (rl-read-observation buffer)))
    (let ((action (agent-step agent reward observation)))
      (buffer-clear buffer)
      (rl-write-action action buffer)))
  agent)

(defun on-agent-end (agent buffer)
  "Handles the buffer operations for agent-end."
  (declare #.*optimize-settings*)
  (let ((reward (rl-read-reward buffer)))
    (agent-end agent reward))
  (buffer-clear buffer)
  agent)

(defun on-agent-cleanup (agent buffer)
  "Handles the buffer operations for agent-cleanup."
  (declare #.*optimize-settings*)
  (agent-cleanup agent)
  (buffer-clear buffer)
  agent)

(defun on-agent-message (agent buffer)
  "Handles the buffer operations for agent-message."
  (declare #.*optimize-settings*)
  (let ((input-msg (rl-read-message buffer)))
    (let ((output-msg (agent-message agent input-msg)))
      (buffer-clear buffer)
      (rl-write-message output-msg buffer)))
  agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-agent-event-loop (agent socket buffer)
  "Communication loop of an agent with the rl-glue server."
  (declare #.*optimize-settings*)
  (loop do
       (buffer-clear buffer)
       (let ((state (rl-recv-buffer socket buffer)))
         (declare (fixnum state))
         (cond
           ((= state +k-agent-init+) (on-agent-init agent buffer))
           ((= state +k-agent-start+) (on-agent-start agent buffer))
           ((= state +k-agent-step+) (on-agent-step agent buffer))
           ((= state +k-agent-end+) (on-agent-end agent buffer))
           ((= state +k-agent-cleanup+) (on-agent-cleanup agent buffer))
           ((= state +k-agent-message+) (on-agent-message agent buffer))
           ((= state +k-rl-term+) (return))
           (t (assert nil (state) "Unknown agent state: ~D" state)))
         (rl-send-buffer socket buffer state)))
  agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-agent (agent
                  &key
                  (host +k-localhost+)
                  (port +k-default-port+)
                  (max-retry nil)
                  (retry-timeout +k-retry-timeout+)
                  (autoreconnect nil))
  "Connects the specified AGENT to RL-Glue on HOST and PORT. If the 
attempt is refused, it is tried again MAX-RETRY times, waiting for 
RETRY-TIMEOUT second between them.

PARAMETERS:
    agent         : agent object in use [agent]
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
  (forced-format "RL-Glue Lisp Agent Codec Version ~a, Build ~a~%"
                 (get-codec-version) (get-vc-codec-version))
  (rl-runner agent +k-agent-connection+ #'run-agent-event-loop
             host port max-retry retry-timeout autoreconnect))

