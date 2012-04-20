
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
;;; $Revision: 588 $
;;; $Date: 2009-02-04 16:20:28 -0500 (Wed, 04 Feb 2009) $

(in-package #:rl-glue-mines-sarsa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sarsa-agent (rl-glue-codec:agent)
  ((policy-frozen
    :accessor policy-frozen
    :initform nil
    :documentation "Enables/disables learning.")
   (exploring-frozen
    :accessor exploring-frozen
    :initform nil
    :documentation "Enables/disables exploring.")
   (alpha
    :accessor alpha
    :initarg :alpha
    :initform (error "Must specify alpha!")
    :documentation "The learning stepsize.")
   (epsilon
    :accessor epsilon
    :initarg :epsilon
    :initform (error "Must specify epsilon!")
    :documentation "Probability of random action selection.")
   (gamma
    :accessor gamma
    :initarg :gamma
    :initform nil
    :documentation "The discount factor.")
   (init-q-value
    :accessor init-q-value
    :initarg :init-q-value
    :initform 0
    :documentation "The initial action-value in the lookup table.")
   (lookup-table
    :accessor lookup-table
    :documentation "Lookup table of state-action values.")
   (list-of-actions
    :accessor list-of-actions
    :documentation "List of the possible actions in all states.")
   (previous-observation
    :accessor previous-observation
    :documentation "Previously observed state.")
   (previous-action
    :accessor previous-action
    :documentation "Previously selected action.")
   (previous-value
    :accessor previous-value
    :documentation "Value of the previously selected action.")
   (rand-state
    :reader rand-state
    :initform (make-random-state t)
    :documentation "The random state used by the agent."))
  (:documentation "Class of the SARSA agent."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check the cl-variates library (http://www.cliki.net/cl-variates)
;; if you need more sophisticated pseudo random number support.

(defun random-0-1 (random-state)
  "Returns a random number between 0 and 1."
  (random 1.0d0 random-state))

(defun random-element (sequence random-state)
  "Returns a randomly selected element from SEQUENCE."
  (elt sequence (random (length sequence) random-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check other libraries for advanced serialization and persistency support.
;; Just to mention a few of them:
;;  - cl-store (http://www.cliki.net/cl-store)
;;  - cl-serializer (http://www.cliki.net/cl-serializer)
;;  - elephant (http://www.cliki.net/Elephant)

(defun make-lookup-table ()
  (make-hash-table :test #'equalp))

(defun make-actions-table ()
  (make-hash-table :test #'equalp))

(defun save-lookup-table (sarsa-agent filename)
  "Saves the value function (lookup-table) into a file."
  (format t "Saving value function...")
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (loop
       for observation being the hash-keys
       in (lookup-table sarsa-agent)
       using (hash-value actions-table)
       do (write (cons observation
                       (loop
                          for action being the hash-keys
                          in actions-table
                          using (hash-value value)
                          collect (cons action value)))
                 :stream stream)))
  (format t "saved.~%")
  sarsa-agent)

(defun load-lookup-table (sarsa-agent filename)
  "Loads the value function (lookup-table) from a file."
  (format t "Loading value function...")
  (with-open-file (stream filename :direction :input)
    (loop
       with lookup-table = (make-lookup-table)
       for entry = (read stream nil)
       while entry
       do (let ((observation (first entry))
                (actions-table (make-actions-table)))
            (setf (gethash observation lookup-table) actions-table)
            (loop
               for (action . value) in (rest entry)
               do (setf (gethash action actions-table) value)))
       finally
         (setf (lookup-table sarsa-agent) lookup-table)))
  (format t "loaded.~%")
  sarsa-agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun adt-to-id (adt)
  "Converts the ADT object to internal lisp data, which can be compared by
 equalp and so can be stored in a hash table."
  (list (rl-glue-codec:int-array adt)
        (rl-glue-codec:float-array adt)
        (rl-glue-codec:char-string adt)))

(defun fetch-q-value (sarsa-agent observation action)
  "Returns the action-value related to OBSERVATION and ACTION from the lookup
 table of SARSA-AGENT. If this OBSERVATION-ACTION pair is observed at the
 first time, the initial action value is returned."
  (let ((observation-id (adt-to-id observation))
        (action-id (adt-to-id action)))
    (let ((actions-table (gethash observation-id (lookup-table sarsa-agent))))
      (or (and actions-table (gethash action-id actions-table))
          (init-q-value sarsa-agent)))))

(defun commit-q-value (sarsa-agent observation action delta)
  "Increases the action-value related to OBSERVATION and ACTION by the amount
 of DELTA scaled by the learning rate parameter."
  (let ((observation-id (adt-to-id observation))
        (action-id (adt-to-id action)))
    (let* ((table (lookup-table sarsa-agent))
           (actions-table (gethash observation-id table)))
      (unless actions-table
        (setf actions-table (make-actions-table))
        (setf (gethash observation-id table) actions-table))
      (let ((value (or (gethash action-id actions-table)
                       (init-q-value sarsa-agent))))
        (setf (gethash action-id actions-table)
              (+ value (* (alpha sarsa-agent) delta))))))
  sarsa-agent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun select-action (sarsa-agent observation)
  "Returns an action selected epsilon-greedily. If there is more than one
 actions has the same maximum value, the first is returned, so no any
 tie-breaking is used."
  (let ((rng-state (rand-state sarsa-agent))
        (actions-list (list-of-actions sarsa-agent)))
    (if (and (not (exploring-frozen sarsa-agent))
             (< (random-0-1 rng-state) (epsilon sarsa-agent)))
        (random-element actions-list rng-state)
        (let* ((max-action (first actions-list))
               (max-value (fetch-q-value sarsa-agent observation max-action)))
          (dolist (action (rest actions-list))
            (let ((value (fetch-q-value sarsa-agent observation action)))
              (when (< max-value value)
                 (setf max-action action)
                 (setf max-value value))))
          max-action))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun store-oav (sarsa-agent observation action value)
  "Stores OBSERVATION, ACTION and VALUE into SARSA-AGENT."
  (setf (previous-observation sarsa-agent) observation)
  (setf (previous-action sarsa-agent) action)
  (setf (previous-value sarsa-agent) value)
  sarsa-agent)

(defun learn-p (sarsa-agent)
  "Returns T if the agent can learn (when information about the previous
 step is available)."
  (previous-observation sarsa-agent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rl-glue-codec:agent-init ((agent sarsa-agent) task-spec)
  (setf (lookup-table agent) (make-lookup-table))
  (store-oav agent nil nil nil)
  ;; processing the task specification
  (let ((ts (rl-glue-utils:parse-task-spec task-spec)))
    (assert (= 1 (length (rl-glue-utils:int-actions ts))))
    (assert (zerop (length (rl-glue-utils:float-actions ts))))
    (assert (zerop (rl-glue-utils:char-actions ts)))
    (let* ((range (aref (rl-glue-utils:int-actions ts) 0))
           (min (rl-glue-utils:min-value range))
           (max (rl-glue-utils:max-value range)))
      (assert (and (numberp min) (numberp max) (< min max)))
      (setf (list-of-actions agent)
            (loop
               for action from min to max
               collect (rl-glue-codec:make-action
                        :int-array (rl-glue-codec:make-int-array
                                    1
                                    :initial-contents (list action))))))
    (unless (gamma agent)
      (setf (gamma agent) (rl-glue-utils:discount-factor ts))))
  agent)

(defmethod rl-glue-codec:agent-start ((agent sarsa-agent) first-observation)
  (let ((action (select-action agent first-observation)))
    (unless (policy-frozen agent)
      (let ((value (fetch-q-value agent first-observation action)))
        (store-oav agent first-observation action value)))
    action))

(defmethod rl-glue-codec:agent-step ((agent sarsa-agent) reward observation)
  (let ((action (select-action agent observation)))
    (unless (policy-frozen agent)
      (let ((value (fetch-q-value agent observation action)))
        (when (learn-p agent)
          (commit-q-value agent
                          (previous-observation agent)
                          (previous-action agent)
                          (- (+ reward
                                (* (gamma agent) value))
                             (previous-value agent))))
        (store-oav agent observation action value)))
    action))

(defmethod rl-glue-codec:agent-end ((agent sarsa-agent) reward)
  (unless (policy-frozen agent)
    (when (learn-p agent)
      (commit-q-value agent
                      (previous-observation agent)
                      (previous-action agent)
                      (- reward (previous-value agent))))
    (store-oav agent nil nil nil))
  agent)

(defmethod rl-glue-codec:agent-cleanup ((agent sarsa-agent))
  (setf (lookup-table agent) nil)
  (store-oav agent nil nil nil)
  agent)

(defmethod rl-glue-codec:agent-message ((agent sarsa-agent) input-message)
  (cond
    ((string= input-message "freeze learning")
     (setf (policy-frozen agent) t)
     (store-oav agent nil nil nil)
     "Message understood. Policy is frozen.")
    ((string= input-message "unfreeze learning")
     (setf (policy-frozen agent) nil)
     "Message understood. Policy is unfrozen.")
    ((string= input-message "freeze exploring")
     (setf (exploring-frozen agent) t)
     "Message understood. Exploring is frozen.")
    ((string= input-message "unfreeze exploring")
     (setf (exploring-frozen agent) nil)
     "Message understood. Exploring is unfrozen.")
    ((and (>= (length input-message) 12)
          (string= (subseq input-message 0 11) "save_policy"))
     (save-lookup-table agent (subseq input-message 12))
     "Message understood. Policy is saved.")
    ((and (>= (length input-message) 12)
          (string= (subseq input-message 0 11) "load_policy"))
     (load-lookup-table agent (subseq input-message 12))
     (store-oav agent nil nil nil)
     "Message understood. Policy is loaded.")
    (t "Sarsa-agent does not understand your message.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-agent (&rest args)
  "Starts a sarsa agent."
  (apply #'rl-glue-codec:run-agent
         (make-instance 'sarsa-agent :alpha 0.1 :epsilon 0.1)
         args))

