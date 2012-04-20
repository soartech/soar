
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

(defclass test-empty-experiment (test-experiment) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-test-empty (exp which-ep observation action
                         &key reward terminal-p)
  "Performs tests during an episode of the test-empty test."
  (if (evenp which-ep)
      (progn
        (check-adt exp action 0 0 0)
        (check-adt exp observation 0 0 0))
      (progn
        (check-adt exp action 7 3 1)
        (check-adt exp observation 2 4 5)))
  (when reward
    (check exp #'= 0.0 reward))
  (when terminal-p
    (check exp #'eq nil terminal-p))
  exp)

(defun run-test-empty-experiment (exp &rest args)
  "Runs the experiment of test-empty test."
  (apply #'rl-init exp args)
  (loop for which-ep from 1 to 4
     do
       (multiple-value-bind (observation action) (rl-start exp)
         (check-test-empty exp which-ep observation action))
       (loop for which-step from 1 to 4
          do
            (multiple-value-bind (reward observation terminal-p action)
                (rl-step exp)
              (check-test-empty exp
                                which-ep
                                observation
                                action
                                :reward reward
                                :terminal-p terminal-p))))
  (summarize-stat exp)
  (rl-close exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-test-empty-experiment (&rest args)
  "Starting a test-empty-experiment experiment."
  (apply #'run-test-empty-experiment
         (make-instance 'test-empty-experiment
                        :test-name "test-empty") args))

