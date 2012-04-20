
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

(defclass test-1-experiment (test-experiment) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro check-test-1-adt (exp adt exp-i0)
  "Checks the ADT values during test-1 test."
  `(progn
     (check ,exp #'= 1 (length (int-array ,adt)))
     (check ,exp #'= 0 (length (float-array ,adt)))
     (check ,exp #'= 0 (length (char-string ,adt)))
     (check ,exp #'= ,exp-i0 (aref (int-array ,adt) 0))))

(defun check-test-1 (exp exp-i0 exp-terminal-p
                     msg exp-agent-msg exp-env-msg)
  "Performs checks during a step for the test-1 test."
  (multiple-value-bind (reward observation terminal-p action) (rl-step exp)
    (check exp #'string= exp-env-msg (rl-env-message exp msg))
    (check exp #'string= exp-agent-msg (rl-agent-message exp msg))
    (check exp #'eq exp-terminal-p terminal-p)
    (when exp-i0
      (check-test-1-adt exp observation exp-i0)
      (check-test-1-adt exp action exp-i0))
    (check exp #'= 1.0d0 reward)))

(defun run-test-1-experiment (exp &rest args)
  "Runs the experiment of test-1 test."
  (apply #'rl-init exp args)
  (rl-start exp)
  (check-test-1 exp 0 nil "one" "one|1.|one" "one|1.|one")
  (check-test-1 exp 1 nil "two" "two|2.2.|two" "two|2.2.|two")
  (check-test-1 exp 2 nil "three" "three||three" "three||three")
  (check-test-1 exp 3 nil "four" "four|4.|four" "four|4.|four")
  (check-test-1 exp nil t "five" "five|4.|five" "five|5.5.|five")
  (multiple-value-bind (reward observation terminal-p action) (rl-step exp)
    (check exp #'= -2.0d0 reward)
    (check exp #'eq nil terminal-p)
    (with-accessors ((int-array int-array)
                     (float-array float-array)
                     (char-string char-string)) observation
      (check exp #'= 5 (length int-array))
      (check exp #'= 5 (length float-array))
      (check exp #'= 5 (length char-string))
      (check exp #'= 173 (aref int-array 0))
      (check exp #'= -173 (aref int-array 1))
      (check exp #'= 2147483647 (aref int-array 2))
      (check exp #'= 0 (aref int-array 3))
      (check exp #'= -2147483648 (aref int-array 4))
      (check exp #'= 0.0078125d0 (aref float-array 0))
      (check exp #'= -0.0078125d0 (aref float-array 1))
      (check exp #'= 0.0d0 (aref float-array 2))
      (check exp #'= 0.0078125d150 (aref float-array 3))
      (check exp #'= -0.0078125d150 (aref float-array 4))
      (check exp #'char= #\g (char char-string 0))
      (check exp #'char= #\F (char char-string 1))
      (check exp #'char= #\? (char char-string 2))
      (check exp #'char= #\Space (char char-string 3))
      (check exp #'char= #\& (char char-string 4)))
    (check exp #'rl-equalp observation action))
  (summarize-stat exp)
  (rl-close exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-test-1-experiment (&rest args)
  "Starting a test-1-experiment experiment."
  (apply #'run-test-1-experiment (make-instance 'test-1-experiment
                                                :test-name "test-1") args))

