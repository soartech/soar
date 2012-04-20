
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
;;; $Revision: 842 $
;;; $Date: 2009-04-29 09:11:45 -0400 (Wed, 29 Apr 2009) $

(in-package #:org.rl-community.rl-glue-codec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline make-typed-array))
(defun make-typed-array (size type &optional initial-contents)
  "Makes an array of SIZE with elements of TYPE."
  (declare #.*optimize-settings*)
  (check-type size fixnum)
  (check-type initial-contents list)
  (assert (or (null initial-contents)
              (= (the fixnum size) (length (the list initial-contents))))
          (size initial-contents))
  (let ((array (make-array (the fixnum size) :element-type type)))
    (declare (type simple-array array))
    (when initial-contents
      (loop
         for e in initial-contents
         for i from 0
         do (setf (aref array i) e)))
    array))

(defun make-int-array (size &key initial-contents)
  "Makes an integer array of SIZE with the package supported integer 
typed elements."
  (declare #.*optimize-settings*)
  (make-typed-array size 'integer-t initial-contents))

(defun make-float-array (size &key initial-contents)
  "Makes a float array of SIZE with the package supported floating point 
typed elements."
  (declare #.*optimize-settings*)
  (make-typed-array size 'double-float initial-contents))

(defparameter *init-integer-array* (make-int-array 0)
  "An empty array typed by the package supported integer numbers.")
(defparameter *init-float-array* (make-float-array 0)
  "An empty array typed by the package supported floating point numbers.")
(declaim (type simple-array +empty-integer-array+ +empty-float-array+))

(defclass rl-abstract-type ()
  ((int-array
    :accessor int-array
    :initarg :int-array
    :initform *init-integer-array*
    :type (simple-array integer-t)
    :documentation "Array of integer numbers.")
   (float-array
    :accessor float-array
    :initarg :float-array
    :initform *init-float-array*
    :type (simple-array double-float)
    :documentation "Array of floating point numbers.")
   (char-string
    :accessor char-string
    :initarg :char-string
    :initform ""
    :type string
    :documentation "Character string."))
  (:documentation "General RL-Glue data representation."))

(defclass observation (rl-abstract-type)
  () (:documentation "General RL-Glue observation data representation."))

(defun make-observation (&rest args)
  "Makes an observation object."
  (declare #.*optimize-settings*)
  (apply #'make-instance 'observation args))

(defclass action (rl-abstract-type)
  () (:documentation "General RL-Glue action data representation."))

(defun make-action (&rest args)
  "Makes an action object."
  (declare #.*optimize-settings*)
  (apply #'make-instance 'action args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric rl-equalp (object-1 object-2)
  (declare #.*optimize-settings*)
  (:documentation "Compares two RL objects."))

(defgeneric rl-read (object byte-stream)
  (declare #.*optimize-settings*)
  (:documentation "Reads an object from BYTE-STREAM."))

(defgeneric rl-write (object byte-stream)
  (declare #.*optimize-settings*)
  (:documentation "Writes an object to BYTE-STREAM."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rl-equalp ((object-1 t) (object-2 t))
  "By default it works than the equalp function."
  (declare #.*optimize-settings*)
  (equalp object-1 object-2))

(defmethod rl-equalp ((object-1 rl-abstract-type) (object-2 rl-abstract-type))
  "Compares two RL abstract data type objects."
  (declare #.*optimize-settings*)
  (and (equalp (int-array object-1) (int-array object-2))
       (equalp (float-array object-1) (float-array object-2))
       (string= (the string (char-string object-1))
                (the string (char-string object-2)))))

(defmethod rl-read ((self rl-abstract-type) buffer)
  "Reads an ADT object from the buffer."
  (declare #.*optimize-settings*)
  (let ((int-num (buffer-read-int buffer))
        (float-num (buffer-read-int buffer))
        (char-num (buffer-read-int buffer)))
    (declare (fixnum int-num float-num char-num))
    (setf (int-array self) (buffer-read-int-seq buffer int-num))
    (setf (float-array self) (buffer-read-float-seq buffer float-num))
    (setf (char-string self) (buffer-read-string buffer char-num)))
  self)

(defmethod rl-write ((self rl-abstract-type) buffer)
  "Writes an ADT object to the buffer."
  (declare #.*optimize-settings*)
  (let ((int-array (int-array self))
        (float-array (float-array self))
        (char-string (char-string self)))
    (check-type int-array (simple-array integer-t))
    (check-type float-array (simple-array double-float))
    (check-type char-string string)
    (let ((int-num (length (the simple-array int-array)))
          (float-num (length (the simple-array float-array)))
          (char-num (length (the string char-string))))
      (declare (fixnum int-num float-num char-num))
      (buffer-write-int int-num buffer)
      (buffer-write-int float-num buffer)
      (buffer-write-int char-num buffer)
      (when (plusp int-num)
        (buffer-write-int-seq int-array buffer int-num nil))
      (when (plusp float-num)
        (buffer-write-float-seq float-array buffer float-num nil))
      (when (plusp char-num)
        (buffer-write-string char-string buffer nil))))
  self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline rl-read-observation))
(defun rl-read-observation (buffer)
  "Reads an observation object from BUFFER."
  (declare #.*optimize-settings*)
  (rl-read (make-instance 'observation) buffer))

(declaim (inline rl-write-observation))
(defun rl-write-observation (observation buffer)
  "Writes OBSERVATION object into BUFFER."
  (declare #.*optimize-settings*)
  (rl-write observation buffer))

(declaim (inline rl-read-action))
(defun rl-read-action (buffer)
  "Reads an action object from BUFFER."
  (declare #.*optimize-settings*)
  (rl-read (make-instance 'action) buffer))

(declaim (inline rl-write-action))
(defun rl-write-action (action buffer)
  "Writes ACTION object into BUFFER."
  (declare #.*optimize-settings*)
  (rl-write action buffer))

(declaim (inline rl-read-reward))
(defun rl-read-reward (buffer)
  "Reads a reward value from BUFFER."
  (declare #.*optimize-settings*)
  (buffer-read-float buffer))

(declaim (inline rl-write-reward))
(defun rl-write-reward (reward buffer)
  "Writes REWARD value into BUFFER."
  (declare #.*optimize-settings*)
  (check-type reward real)
  (buffer-write-float (coerce reward 'double-float) buffer))

(declaim (inline rl-read-message))
(defun rl-read-message (buffer)
  "Reads a message string from BUFFER."
  (declare #.*optimize-settings*)
  (buffer-read-string buffer))

(declaim (inline rl-write-message))
(defun rl-write-message (message buffer)
  "Writes MESSAGE string into BUFFER."
  (declare #.*optimize-settings*)
  (check-type message string)
  (buffer-write-string message buffer))

(declaim (inline rl-read-task-spec))
(defun rl-read-task-spec (buffer)
  "Reads a task specification string from BUFFER."
  (declare #.*optimize-settings*)
  (buffer-read-string buffer))

(declaim (inline rl-write-task-spec))
(defun rl-write-task-spec (task-spec buffer)
  "Writes TASK-SPEC task specification string into BUFFER."
  (declare #.*optimize-settings*)
  (check-type task-spec string)
  (buffer-write-string task-spec buffer))

(declaim (inline rl-read-terminal))
(defun rl-read-terminal (buffer)
  "Reads a terminal indicator boolean from BUFFER."
  (declare #.*optimize-settings*)
  (ecase (buffer-read-int buffer)
    ((0) nil)
    ((1) t)))

(declaim (inline rl-write-terminal))
(defun rl-write-terminal (terminal buffer)
  "Writes TERMINAL terminal indicator boolean into BUFFER."
  (declare #.*optimize-settings*)
  (check-type terminal boolean)
  (let ((boolint (if terminal 1 0)))
    (buffer-write-int boolint buffer)))

