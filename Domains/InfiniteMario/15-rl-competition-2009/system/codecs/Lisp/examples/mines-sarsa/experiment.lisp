

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

(defclass evaluation-point ()
  ((mean
    :reader mean
    :initarg :mean
    :documentation "Mean of rewards.")
   (standard-deviation
    :reader standard-deviation
    :initarg :standard-deviation
    :documentation "Standard deviation of rewards."))
  (:documentation "Statistics data of evaluation points."))

(defclass mines-sarsa-experiment (rl-glue-codec:experiment)
  ()
  (:documentation "Experiment for the mines-sarsa example."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluate-agent (experiment)
  "Evaluates the agent's performance and returns the mean and the standard
 deviation of the collected total reward."
  (let ((n 10))
    (rl-glue-codec:rl-agent-message experiment "freeze learning")
    (loop
       with sum = 0
       with sq-sum = 0
       repeat n
       do
         (rl-glue-codec:rl-episode experiment 5000)
         (let ((this-return (rl-glue-codec:rl-return experiment)))
           (incf sum this-return)
           (incf sq-sum (* this-return this-return)))
       finally
         (rl-glue-codec:rl-agent-message experiment "unfreeze learning")
         (let* ((mean (/ sum n))
                (variance (/ (- sq-sum (* n mean mean)) (1- n))))
           (return (make-instance 'evaluation-point
                                  :mean mean
                                  :standard-deviation (sqrt variance)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-result-csv (statistics filespec)
  "Saves the STATISTICS of an evaluation into a file specified by FILESPEC."
  (with-open-file (stream filespec :direction :output :if-exists :supersede)
    (format stream (concatenate 'string
                                "# Results from experiment. "
                                "First line is means, "
                                "second line is standard deviations.~%"))
    (loop
       for score in statistics
       do (format stream "~,2f," (mean score))
       finally (format stream "~%"))
    (loop
       for score in statistics
       do (format stream "~,2f," (standard-deviation score))
       finally (format stream "~%"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-score (after-episodes evaluation-score)
  "Prints EVALUATION-SCORE to the standard output."
  (format t "~d~15t~,2f~30t~,2f~%"
          after-episodes
          (mean evaluation-score)
          (standard-deviation evaluation-score))
  nil)

(defun offline-demo (experiment)
  "Learns the agent and evaluates it after every 25 episodes."
  (flet ((perform-evaluation (episode-counter)
           (let ((score (evaluate-agent experiment)))
             (print-score episode-counter score)
             score)))
    (loop
       with statistics = '()
       initially
         (push (perform-evaluation 0) statistics)
       for eval-counter from 1 to 20
       do
         (let ((learn-length 25))
           (loop
              repeat learn-length
              do (rl-glue-codec:rl-episode experiment))
           (push (perform-evaluation (* learn-length eval-counter))
                 statistics))
       finally
         (save-result-csv (nreverse statistics) "results.csv")))
  experiment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun printer (&rest texts)
  "Prints all the TEXTS to standard output."
  (format t (apply #'concatenate 'string texts)))

(defun run-experiment (experiment &rest args)
  "Runs the RL-Glue EXPERIMENT initialized by ARGS arguments."
  (apply #'rl-glue-codec:rl-init experiment args)
  (printer "Starting offline demo~%"
           "---------------------~%"
           "Will alternate learning for 25 episodes, "
           "then freeze policy and evaluate for 10 episodes.~%~%")
  (printer "After Episode  Mean Return    Standard Deviation~%"
           "------------------------------------------------~%")
  (offline-demo experiment)
  (printer "~%Now we will save the agent's learned "
           "value function to a file.~%")
  (rl-glue-codec:rl-agent-message experiment "save_policy results.dat")
  (printer "~%Calling rl-cleanup and rl-init to "
           "clear the agent's memory.~%")
  (rl-glue-codec:rl-cleanup experiment)
  (apply #'rl-glue-codec:rl-init experiment args)
  (printer "Evaluating the agent's default policy:~%"
           "               Mean Return    Standard Deviation~%"
           "------------------------------------------------~%")
  (print-score 0 (evaluate-agent experiment))
  (printer "~%Loading up the value function we saved earlier.~%")
  (rl-glue-codec:rl-agent-message experiment "load_policy results.dat")
  (printer "Evaluating the agent after loading the value function:~%"
           "               Mean Return    Standard Deviation~%"
           "------------------------------------------------~%")
  (print-score 0 (evaluate-agent experiment))
  (printer "Telling the environment to use fixed start state of 2,3.~%")
  (rl-glue-codec:rl-env-message experiment "set-start-state 2 3")
  (rl-glue-codec:rl-start experiment)
  (printer "Telling the environment to print "
           "the current state to the screen.~%")
  (rl-glue-codec:rl-env-message experiment "print-state")
  (printer "~%Evaluating the agent a few times from a fixed start "
           "state of 2,3:~%"
           "               Mean Return    Standard Deviation~%"
           "------------------------------------------------~%")
  (print-score 0 (evaluate-agent experiment))
  (printer "~%Evaluating the agent again with the random start state:~%"
           "               Mean Return    Standard Deviation~%"
           "------------------------------------------------~%")
  (rl-glue-codec:rl-env-message experiment "set-random-start-state")
  (print-score 0 (evaluate-agent experiment))
  (printer "~%Program complete.~%")
  (rl-glue-codec:rl-cleanup experiment)
  (rl-glue-codec:rl-close experiment)
  experiment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-experiment (&rest args)
  "Starts a mines-sarsa experiment."
  (apply #'run-experiment (make-instance 'mines-sarsa-experiment) args))

