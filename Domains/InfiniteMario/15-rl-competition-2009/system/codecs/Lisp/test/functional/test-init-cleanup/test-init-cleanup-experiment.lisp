
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

(defclass test-init-cleanup-experiment (test-experiment) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-test-init-cleanup (exp step-num exp-terminal-p exp-step-num)
  (let ((terminal-p (rl-episode exp step-num)))
    (check exp #'eq exp-terminal-p terminal-p)
    (check exp #'= exp-step-num (rl-num-steps exp))))

(defun run-test-init-cleanup-experiment (exp &rest args)
  "Runs the experiment of test-init-cleanup test."
  (apply #'rl-init exp args)
  (check-test-init-cleanup exp 0 t 5)
  (check-test-init-cleanup exp 1 nil 1)
  (rl-cleanup exp)
  (apply #'rl-init exp args)
  (check-test-init-cleanup exp 2 nil 2)
  (check-test-init-cleanup exp 4 nil 4)
  (check-test-init-cleanup exp 5 nil 5)
  (rl-cleanup exp)
  (rl-cleanup exp)
  (apply #'rl-init exp args)
  (check-test-init-cleanup exp 6 t 5)
  (check-test-init-cleanup exp 7 t 5)
  (rl-cleanup exp)
  (summarize-stat exp)
  (rl-close exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-test-init-cleanup-experiment (&rest args)
  "Starting a test-init-cleanup-experiment experiment."
  (apply #'run-test-init-cleanup-experiment
         (make-instance 'test-init-cleanup-experiment
                        :test-name "test-init-cleanup") args))

