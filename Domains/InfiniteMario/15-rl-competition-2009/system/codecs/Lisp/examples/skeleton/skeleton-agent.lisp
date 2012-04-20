
;;; Copyright 2009 Gabor Balazs
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
;;; $Revision: 720 $
;;; $Date: 2009-02-10 09:32:33 -0500 (Tue, 10 Feb 2009) $

(in-package :rl-glue-skeleton)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass skeleton-agent (rl-glue-codec:agent)
  ((rand-state
    :reader rand-state
    :initarg :rand-state
    :initform (make-random-state t)
    :documentation "The random state used by the agent.")
   (last-observation
    :accessor last-observation
    :initform nil
    :documentation "Last observed state.")
   (last-action
    :accessor last-action
    :initform nil
    :documentation "Last selected action."))
  (:documentation "A very thin RL agent skeleton."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-random-action ()
  "Return a random RL-Glue action."
  (rl-glue-codec:make-action
   :int-array (rl-glue-codec:make-int-array
               1 :initial-contents (list (random 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rl-glue-codec:agent-init ((agent skeleton-agent) task-spec)
  agent)

(defmethod rl-glue-codec:agent-start ((agent skeleton-agent) first-observation)
  (let ((action (get-random-action)))
    (setf (last-observation agent) first-observation)
    (setf (last-action agent) action)
    action))

(defmethod rl-glue-codec:agent-step ((agent skeleton-agent) reward observation)
  (declare (ignore reward))
  (let ((action (get-random-action)))
    (setf (last-observation agent) observation)
    (setf (last-action agent) action)
    action))

(defmethod rl-glue-codec:agent-end ((agent skeleton-agent) reward)
  (declare (ignore reward))
  agent)

(defmethod rl-glue-codec:agent-cleanup ((agent skeleton-agent))
  (setf (last-observation agent) nil)
  (setf (last-action agent) nil)
  agent)

(defmethod rl-glue-codec:agent-message ((agent skeleton-agent) input-message)
  (if (string= input-message "What is your name?")
      "My name is skeleton agent, Lisp edition!"
      "I don't know how to respond to your message!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-agent (&rest args)
  "Starts a skeleton agent."
  (apply #'rl-glue-codec:run-agent (make-instance 'skeleton-agent) args))

