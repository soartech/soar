
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

(defclass test-empty-agent (agent)
  ((empty-action
    :accessor empty-action
    :documentation "A fixed empty action.")
   (non-empty-action
    :accessor non-empty-action
    :documentation "A fixed non-empty-action.")
   (which-episode
    :accessor which-episode
    :documentation "Number of the current episode."))
  (:documentation "An agent which sends an empty action in every second 
episodes and a non-empty one in the other cases."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod agent-init ((agent test-empty-agent) task-spec)
  (setf (which-episode agent) 0)
  (setf (empty-action agent) (make-action))
  (setf (non-empty-action agent)
        (fill-adt (make-action) :ints 7 :floats 3 :chars 1))
  agent)

(defun get-test-empty-action (agent)
  (if (evenp (which-episode agent))
      (empty-action agent)
      (non-empty-action agent)))

(defmethod agent-start ((agent test-empty-agent) first-observation)
  (incf (which-episode agent))
  (get-test-empty-action agent))

(defmethod agent-step ((agent test-empty-agent) reward observation)
  (get-test-empty-action agent))

(defmethod agent-end ((agent test-empty-agent) reward)
  agent)

(defmethod agent-cleanup ((agent test-empty-agent))
  agent)

(defmethod agent-message ((agent test-empty-agent) input-message)
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-test-empty-agent (&rest args)
  "Starting a test-empty-agent agent."
  (apply #'run-agent (make-instance 'test-empty-agent) args))

