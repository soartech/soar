
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
;;; $Revision: 732 $
;;; $Date: 2009-02-10 16:27:50 -0500 (Tue, 10 Feb 2009) $

(in-package :rl-glue-skeleton)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass skeleton-experiment (rl-glue-codec:experiment)
  ((which-episode
    :accessor which-episode
    :initform 0
    :documentation "The episode counter."))
  (:documentation "A very thin skeleton experiment."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-episode (exp step-limit)
  "Runs one episode of length maximum cutoff."
  (let* ((terminal (rl-glue-codec:rl-episode exp step-limit))
         (total-steps (rl-glue-codec:rl-num-steps exp))
         (total-reward (rl-glue-codec:rl-return exp)))
    (format t "Episode ~d ~15t~d steps ~30t~f reward ~50t~d natural end~%"
            (which-episode exp)
            total-steps
            total-reward
            (if terminal 1 0)))
  (incf (which-episode exp)))

(defun run-experiment (exp &rest args)
  "Runs the specified skeleton experiment."
  (format t "~%~%Experiment starting up!~%")
  (let ((task-spec-string (apply #'rl-glue-codec:rl-init exp args)))
    (format t "rl-init called, the environment sent task spec: ~a~%"
            task-spec-string))
  (format t "~%~%----------Sending some sample messages----------~%")
  ;; Talk to the agent and environment a bit...
  (let ((response-message (rl-glue-codec:rl-agent-message
                           exp "What is your name?")))
    (format t "Agent responded to \"What is your name?\" with: ~a~%"
            response-message))
  (let ((response-message (rl-glue-codec:rl-agent-message
                           exp (concatenate 'string
                                            "If at first you don't succeed; "
                                            "call it version 1.0"))))
    (format t (concatenate 'string
                           "Agent responded to \"If at first you don't "
                           "succeed; call it version 1.0\" with: ~a~%")
            response-message))
  (let ((response-message (rl-glue-codec:rl-env-message
                           exp "What is your name?")))
    (format t "~%Environment responded to \"What is your name?\" with: ~a~%"
            response-message))
  (let ((response-message (rl-glue-codec:rl-env-message
                           exp (concatenate 'string
                                            "If at first you don't succeed; "
                                            "call it version 1.0"))))
    (format t (concatenate 'string
                           "Environment responded to \"If at first you don't "
                           "succeed; call it version 1.0\" with: ~a~%")
            response-message))
  (format t "~%~%----------Running a few episodes----------~%")
  (run-episode exp 100)
  (run-episode exp 100)
  (run-episode exp 100)
  (run-episode exp 100)
  (run-episode exp 100)
  (run-episode exp 1)
  ;; Remember that stepLimit of 0 means there is no limit at all!
  (run-episode exp 0)
  (rl-glue-codec:rl-cleanup exp)
  (format t "~%~%----------Stepping through an episode----------~%")
  ;; We could also start over and do another experiment.
  (apply #'rl-glue-codec:rl-init exp args)
  ;; We could run one step at a time instead of one episode at a time.
  ;; Start the episode.
  (multiple-value-bind (observation action)
      (rl-glue-codec:rl-start exp)
    (format t "First observation and action were: ~d and: ~d~%"
            (aref (rl-glue-codec:int-array observation) 0)
            (aref (rl-glue-codec:int-array action) 0)))
  ;; Run one step.
  (rl-glue-codec:rl-step exp)
  ;; Run until the episode ends.
  (loop do
       (multiple-value-bind (reward observation terminal action)
           (rl-glue-codec:rl-step exp)
         (declare (ignore reward))
         (if terminal
             (return)
             ;; Could optionally print state,action pairs.
             (format t "(~d,~d) "
                     (aref (rl-glue-codec:int-array observation) 0)
                     (aref (rl-glue-codec:int-array action) 0)))))
  (format t "~&~%~%----------Summary----------~%")
  (let* ((total-steps (rl-glue-codec:rl-num-steps exp))
         (total-reward (rl-glue-codec:rl-return exp)))
    (format t "It ran for ~d steps, total reward was: ~f~%"
            total-steps total-reward))
  (rl-glue-codec:rl-cleanup exp)
  (rl-glue-codec:rl-close exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-experiment (&rest args)
  "Starts a skeleton experiment."
  (apply #'run-experiment (make-instance 'skeleton-experiment) args))

