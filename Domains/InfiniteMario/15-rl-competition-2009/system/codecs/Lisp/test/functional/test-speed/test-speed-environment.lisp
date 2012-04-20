
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

(defclass test-speed-environment (environment)
  ((episode-count
    :accessor episode-count
    :documentation "Episode counter.")
   (step-count
    :accessor step-count
    :documentation "Step counter in an episode.")
   (observation
    :accessor observation
    :initform (make-observation)
    :documentation "Current observation."))
  (:documentation "An environment which creates larger observations and 
shorter episodes for every even and smaller observations with longer 
episodes for every odd episodes."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod env-init ((env test-speed-environment))
  (setf (episode-count env) 0)
  "sample task spec")

(defmethod env-start ((env test-speed-environment))
  (incf (episode-count env))
  (setf (step-count env) 0)
  (let ((observation (observation env))
        (num (if (evenp (episode-count env)) 50000 5)))
    (setf (int-array observation) (make-int-array num))
    (setf (float-array observation) (make-float-array num))
    (setf (observation env) observation)
    (fill-adt observation :ints num :floats num)))

(defmethod env-step ((env test-speed-environment) action)
  (with-accessors ((step-count step-count)) env
    (incf step-count)
    (let ((observation (observation env)))
      (if (evenp (episode-count env))
          (values 1.0d0
                  (fill-adt observation :ints 50000 :floats 50000)
                  (= step-count 200))
          (values 1.0d0
                  (fill-adt observation :ints 5 :floats 5)
                  (= step-count 5000))))))

(defmethod env-cleanup ((env test-speed-environment))
  env)

(defmethod env-message ((env test-speed-environment) input-message)
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-test-speed-environment (&rest args)
  "Starting a test-speed-environment environment."
  (apply #'run-env (make-instance 'test-speed-environment) args))

