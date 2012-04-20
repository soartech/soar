
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
;;; $Revision: 743 $
;;; $Date: 2009-02-13 05:08:26 -0500 (Fri, 13 Feb 2009) $

(in-package #:rl-glue-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-int-range-array (size &key initial-contents)
  "Makes an array of SIZE with the package supported int-range elements."
  (rl-glue-codec::make-typed-array size 'int-range initial-contents))

(defun make-float-range-array (size &key initial-contents)
  "Makes an array of SIZE with the package supported float-range elements."
  (rl-glue-codec::make-typed-array size 'float-range initial-contents))

(defclass task-spec ()
  ((version
    :accessor version
    :initarg :version
    :initform "RL-Glue-3.0"
    :type string
    :documentation "Version name of the task specification language.")
   (problem-type
    :accessor problem-type
    :initarg :problem-type
    :initform ""
    :type string
    :documentation "Type of the problem to be solved.")
   (discount-factor
    :accessor discount-factor
    :initarg :discount-factor
    :initform 0.0
    :type (float 0.0 1.0)
    :documentation "Discount factor.")
   (int-observations
    :accessor int-observations
    :initarg :int-observations
    :initform (make-int-range-array 0)
    :type (simple-array int-range)
    :documentation "Observation ranges with integer values.")
   (float-observations
    :accessor float-observations
    :initarg :float-observatinos
    :initform (make-float-range-array 0)
    :type (simple-array float-range)
    :documentation "Observation ranges with floating point values.")
   (char-observations
    :accessor char-observations
    :initarg :char-observations
    :initform 0
    :type (integer 0 *)
    :documentation "Number of characters specified for observations.")
   (int-actions
    :accessor int-actions
    :initarg :int-actions
    :initform (make-int-range-array 0)
    :type (simple-array int-range)
    :documentation "Action ranges with integer values.")
   (float-actions
    :accessor float-actions
    :initarg :float-actions
    :initform (make-float-range-array 0)
    :type (simple-array float-range)
    :documentation "Action ranges with floating point values.")
   (char-actions
    :accessor char-actions
    :initarg :char-actions
    :initform 0
    :type (integer 0 *)
    :documentation "Number of characters specified for actions.")
   (rewards
    :accessor rewards
    :initarg :rewards
    :initform (make-float-range)
    :type float-range
    :documentation "Range of rewards.")
   (extra-spec
    :accessor extra-spec
    :initarg :extra-spec
    :initform ""
    :type string
    :documentation "An optional extra specification."))
  (:documentation "Task specification parameters."))

(defmethod initialize-instance :after ((task-spec task-spec)
                                       &key
                                       episodic
                                       continuing)
  (with-accessors ((problem-type problem-type)) task-spec
    (assert (or (not episodic) (not continuing)) (problem-type)
            "Ambiguous problem-type definition!")
    (cond
      (episodic (setf problem-type "episodic"))
      (continuing (setf problem-type "continuing"))))
  task-spec)

(defun make-task-spec (&rest args)
  "Creates a task-spec object."
  (apply #'make-instance 'task-spec args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-version ()
  "Parses the version of the task specification language."
  (next-token)
  (expect-token "VERSION")
  (next-token)
  (expect-token "RL-Glue-3.0")
  (setf (version (get-spec)) (get-token)))

(defun parse-problem-type ()
  "Parses the type of the specified problem."
  (next-token)
  (expect-token "PROBLEMTYPE")
  (next-token)
  (setf (problem-type (get-spec)) (get-token)))

(defun parse-discount-factor ()
  "Parses the discount factor of the problem."
  (next-token)
  (expect-token "DISCOUNTFACTOR")
  (next-token)
  (setf (discount-factor (get-spec))
        (get-token-as-float :min 0 :max 1)))

(defmacro parse-observations-or-actions (expected-token
                                         int-ranges-fn
                                         float-ranges-fn
                                         char-count-fn)
  "Parses the observations or the actions specification parts."
  `(progn
     (next-token)
     (expect-token ,expected-token)
     (next-token)
     (when (string= (get-token) "INTS")
       (setf (,int-ranges-fn (get-spec))
             (collect-int-ranges))
       (next-token))
     (when (string= (get-token) "DOUBLES")
       (setf (,float-ranges-fn (get-spec))
             (collect-float-ranges))
       (next-token))
     (when (string= (get-token) "CHARCOUNT")
       (next-token)
       (setf (,char-count-fn (get-spec))
             (get-token-as-integer))
       (next-token))
     (putback-token)))

(defun parse-observations ()
  "Parses the observation specification part."
  (parse-observations-or-actions "OBSERVATIONS"
                                 int-observations
                                 float-observations
                                 char-observations))

(defun parse-actions ()
  "Parses the actions specification part."
  (parse-observations-or-actions "ACTIONS"
                                 int-actions
                                 float-actions
                                 char-actions))

(defun parse-rewards ()
  "Parses the reward specification part."
  (next-token)
  (expect-token "REWARDS")
  (next-token)
  (setf (rewards (get-spec))
        (parse-float-range)))

(defun parse-extra ()
  "Parses the extra specification."
  (next-token)
  (expect-token "EXTRA")
  (setf (extra-spec (get-spec))
        (string-left-trim *space-char-bag*
                          (get-spec-string))))

(defun parse-task-spec (task-spec-string)
  "Parses TASK-SPEC-STRING and stores it into a task-spec structure.

PARAMETERS:
  task-spec-string : task specification string [string]

RETURNS:
  task specification object [task-spec]"
  (let ((*parser* (make-parser :spec-string task-spec-string)))
    (parse-version)
    (parse-problem-type)
    (parse-discount-factor)
    (parse-observations)
    (parse-actions)
    (parse-rewards)
    (parse-extra)
    (get-spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod to-string ((object task-spec))
  "Returns the string representation of a task-spec object."
  (with-output-to-string (s)
    (flet ((format-ranges (tag range-vector)
             "Formats the range elements of a range vector."
             (when (plusp (length range-vector))
               (format s " ~a" tag)
               (loop for range across range-vector
                  do (format s " ~a" (to-string range)))))
           (format-charcount (char-count)
             "Formats a char count element."
             (when (plusp char-count)
               (format s " CHARCOUNT ~d" char-count))))
      (format s "VERSION ~a" (version object))
      (format s " PROBLEMTYPE ~a" (problem-type object))
      (format s " DISCOUNTFACTOR ~f"
              (coerce (discount-factor object) 'single-float))
      (format s " OBSERVATIONS")
      (format-ranges "INTS" (int-observations object))
      (format-ranges "DOUBLES" (float-observations object))
      (format-charcount (char-observations object))
      (format s " ACTIONS")
      (format-ranges "INTS" (int-actions object))
      (format-ranges "DOUBLES" (float-actions object))
      (format-charcount (char-actions object))
      (format s " REWARDS ~a" (to-string (rewards object)))
      (format s " EXTRA ~a" (extra-spec object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun across-ranges (fn range-vector)
  "Goes across a RANGE-VECTOR and applies the FN function to each range with
 handling the repeat-count transparently. It means that FN is called by the
 minimum and maximum value of the range as many times as the repeat-count of
 the currently processed range. The result of the function calls are collected
 into a list.

PARAMETERS:
  fn           : function which is called with every range min and max value
                 [function : min max -> result (number number -> t)]
  range-vector : vector of ranges on which processing happens [vector (range)]

RESULT:
  result list of the function calls [list]"
  (loop
     for range across range-vector
     nconc (loop
              repeat (repeat-count range)
              collect (funcall fn (min-value range) (max-value range)))))

(defun ranges-dimension (range-vector)
  "Returns the range vector dimension by summarizing the repeat counts.

PARAMETERS:
  range-vector : vector of ranges which dimension is asked [vector (range)]

RETURNS:
  dimension [0 <= integer]"
  (loop
     for range across range-vector
     sum (repeat-count range)))

