
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

(defclass test-1-environment (environment)
  ((step-count
    :accessor step-count
    :initform 0
    :documentation "Step counter."))
  (:documentation "A simple never terminating environment."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod env-init ((env test-1-environment))
  "sample task spec")

(defmethod env-start ((env test-1-environment))
  (setf (step-count env) 0)
  (fill-adt (make-observation) :ints 1 :floats 2 :chars 3))

(defmethod env-step ((env test-1-environment) action)
  (with-accessors ((step-count step-count)) env
    (if (< step-count 5)
        (let ((observation (fill-adt (make-observation) :ints 1)))
          (setf (aref (int-array observation) 0) step-count)
          (incf step-count)
          (values 1.0d0 observation (= step-count 5)))
        (let ((observation (fill-adt (make-observation) :ints 5 :floats 5 :chars 5)))
          (with-accessors ((int-array int-array)
                           (float-array float-array)
                           (char-string char-string)) observation
            (setf (aref int-array 0) 173
                  (aref int-array 1) -173
                  (aref int-array 2) 2147483647
                  (aref int-array 3) 0
                  (aref int-array 4) -2147483648)
            (setf (aref float-array 0) 0.0078125d0
                  (aref float-array 1) -0.0078125d0
                  (aref float-array 2) 0.0d0
                  (aref float-array 3) 0.0078125d150
                  (aref float-array 4) -0.0078125d150)
            (setf (char char-string 0) #\g
                  (char char-string 1) #\F
                  (char char-string 2) #\?
                  (char char-string 3) #\Space
                  (char char-string 4) #\&))
          (values -2 observation nil)))))

(defmethod env-cleanup ((env test-1-environment))
  env)

(defmethod env-message ((env test-1-environment) input-message)
  (create-answer-message (step-count env) input-message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-test-1-environment (&rest args)
  "Starting a test-1-environment environment."
  (apply #'run-env (make-instance 'test-1-environment) args))

