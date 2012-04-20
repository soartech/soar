
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
;;; $Revision: 426 $
;;; $Date: 2009-01-18 15:51:56 -0500 (Sun, 18 Jan 2009) $

(in-package #:rl-glue-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass test-speed-experiment (test-experiment) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-speed-measurement (exp)
  "Measures the speed of an episode run."
  (let ((start-time (get-internal-real-time)))
    (rl-episode exp 0)
    (let ((ms (float (/ (* 1000 (- (get-internal-real-time) start-time))
                        internal-time-units-per-second))))
      (format t "~a Elapsed time in ms: ~f, per step is ~f~%"
                *test-prefix* ms (/ ms (rl-num-steps exp))))))

(defun run-test-speed-experiment (exp &rest args)
  "Runs the experiment of test-speed test."
  (apply #'rl-init exp args)
  (rl-episode exp 500)
  (test-speed-measurement exp)
  (test-speed-measurement exp)
  (rl-close exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-test-speed-experiment (&rest args)
  "Starting a test-speed-experiment experiment."
  (time (apply #'run-test-speed-experiment
               (make-instance 'test-speed-experiment
                              :test-name "test-speed") args)))

