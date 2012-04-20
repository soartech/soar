
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

(defclass test-rl-episode-experiment (test-experiment) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-test-rl-episode-experiment (exp &rest args)
  "Runs the experiment of test-rl-episode test."
  (apply #'rl-init exp args)
  (check exp #'eq t (rl-episode exp 0))
  (check exp #'= 5 (rl-num-steps exp))
  (check exp #'eq nil (rl-episode exp 1))
  (check exp #'= 1 (rl-num-steps exp))
  (check exp #'eq nil (rl-episode exp 2))
  (check exp #'= 2 (rl-num-steps exp))
  (check exp #'eq nil (rl-episode exp 4))
  (check exp #'= 4 (rl-num-steps exp))
  (check exp #'eq nil (rl-episode exp 5))
  (check exp #'= 5 (rl-num-steps exp))
  (check exp #'eq t (rl-episode exp 6))
  (check exp #'= 5 (rl-num-steps exp))
  (check exp #'eq t (rl-episode exp 7))
  (check exp #'= 5 (rl-num-steps exp))
  (summarize-stat exp)
  (rl-close exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-test-rl-episode-experiment (&rest args)
  "Starting a test-rl-episode-experiment experiment."
  (apply #'run-test-rl-episode-experiment
         (make-instance 'test-rl-episode-experiment
                        :test-name "test-rl-episode") args))

