
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

(defclass test-message-experiment (test-experiment) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-test-message-experiment (exp &rest args)
  "Runs the experiment of test-message test."
  (apply #'rl-init exp args)
  (check exp #'string= "empty" (rl-env-message exp ""))
  (check exp #'string= "empty" (rl-agent-message exp ""))
  (check exp #'string= "" (rl-env-message exp "empty"))
  (check exp #'string= "" (rl-agent-message exp "empty"))
  (check exp #'string= "" (rl-env-message exp "null"))
  (check exp #'string= "" (rl-agent-message exp "null"))
  (check exp #'string= "1" (rl-env-message exp "1"))
  (check exp #'string= "1" (rl-agent-message exp "1"))
  (check exp #'string= "1000000000000000000000"
         (rl-env-message exp "1000000000000000000000"))
  (check exp #'string= "1000000000000000000000"
         (rl-agent-message exp "1000000000000000000000"))
  (let ((str-to-check (concatenate 'string
                                   "21111111111111111111111111111111111111111"
                                   "11111111111111131111111111111111111111111"
                                   "1111111111111111111111111111113")))
    (check exp #'string= str-to-check (rl-env-message exp str-to-check)))
  (let ((str-to-check (concatenate 'string
                                   "45555555555555555555555555555555555555555"
                                   "55555555555555565555555555555555555555555"
                                   "5555555555555555555555555555559")))
    (check exp #'string= str-to-check (rl-agent-message exp str-to-check)))
  (summarize-stat exp)
  (rl-close exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-test-message-experiment (&rest args)
  "Starting a test-message-experiment experiment."
  (apply #'run-test-message-experiment
         (make-instance 'test-message-experiment
                        :test-name "test-message") args))

