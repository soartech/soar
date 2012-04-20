
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

(defclass test-1-agent (agent)
  ((step-count
    :accessor step-count
    :documentation "Step counter."))
  (:documentation "Test agent which mostly sends back what it recieves."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod agent-init ((agent test-1-agent) task-spec)
  agent)

(defmethod agent-start ((agent test-1-agent) first-observation)
  (setf (step-count agent) 0)
  ;; returns the observation as the action
  first-observation)

(defmethod agent-step ((agent test-1-agent) reward observation)
  (incf (step-count agent))
  ;; returns the observation as the action
  observation)

(defmethod agent-end ((agent test-1-agent) reward)
  agent)

(defmethod agent-cleanup ((agent test-1-agent))
  agent)

(defmethod agent-message ((agent test-1-agent) input-message)
  (create-answer-message (step-count agent) input-message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-test-1-agent (&rest args)
  "Starting a test-1-agent agent."
  (apply #'run-agent (make-instance 'test-1-agent) args))

