
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

(in-package #:rl-glue-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass test-message-agent (agent)
  ((empty-action
    :accessor empty-action
    :documentation "A fixed empty action."))
  (:documentation "An agent which always sends empty action, but 
tests the message transmitting mechanism."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod agent-init ((agent test-message-agent) task-spec)
  (setf (empty-action agent) (make-action))
  agent)

(defmethod agent-start ((agent test-message-agent) first-observation)
  (empty-action agent))

(defmethod agent-step ((agent test-message-agent) reward observation)
  (empty-action agent))

(defmethod agent-end ((agent test-message-agent) reward)
  agent)

(defmethod agent-cleanup ((agent test-message-agent))
  agent)

(defmethod agent-message ((agent test-message-agent) input-message)
  (cond
    ((string= input-message "") "empty")
    ((string= input-message "empty") "")
    ((string= input-message "null") "")
    (t input-message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-test-message-agent (&rest args)
  "Starting a test-message-agent agent."
  (apply #'run-agent (make-instance 'test-message-agent) args))

