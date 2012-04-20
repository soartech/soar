
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
;;; $Revision: 734 $
;;; $Date: 2009-02-11 15:28:13 -0500 (Wed, 11 Feb 2009) $

(in-package #:rl-glue-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass range ()
  ((repeat-count
    :accessor repeat-count
    :initarg :repeat-count
    :initform 1
    :type (integer 0 *)
    :documentation "How many times the range is repeated."))
  (:documentation "Superclass of range classes."))

(defmacro make-range (type-name number-type)
  "Creates range classes with NUMBER-TYPE endpoints."
  `(defclass ,type-name (range)
     ((min-value
       :accessor min-value
       :initarg :min-value
       :initform 'unspec
       :type (or ,number-type (member -inf unspec))
       :documentation "Minimum of the range.")
      (max-value
       :accessor max-value
       :initarg :max-value
       :initform 'unspec
       :type (or ,number-type (member +inf unspec))
       :documentation "Maximum of the range."))
     (:documentation "Number range.")))

(make-range int-range integer)

(defun make-int-range (&rest args)
  "Creates an int-range object."
  (apply #'make-instance 'int-range args))

(make-range float-range float)

(defun make-float-range (&rest args)
  "Creates a float-range object."
  (apply #'make-instance 'float-range args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-range-bound (source-string bound-type min)
  "Reads a range (min or max) bound value of BOUND-TYPE from SOURCE-STRING."
  (let ((*read-default-float-format* 'double-float))
    (cond
      ((string= source-string "UNSPEC") 'unspec)
      ((and min (string= source-string "NEGINF")) '-inf)
      ((and (not min) (string= source-string "POSINF")) '+inf)
      (t (let ((object (read-from-string source-string)))
           (coerce object bound-type))))))

(defun parse-range (range bound-type)
  "Parses values into the RANGE object and returns it."
  (let ((token-list (parse-tokens-into-list
                     (string-trim *space-char-bag*
                                  (subseq (concatenate 'string
                                                       (get-token)
                                                       " "
                                                       (next-token ")")) 1)))))
    (assert (<= 2 (length token-list)))
    (when (= (length token-list) 3)
      (setf (repeat-count range) (parse-integer (pop token-list))))
    (setf (min-value range)
          (read-range-bound (pop token-list) bound-type t))
    (setf (max-value range)
          (read-range-bound (pop token-list) bound-type nil)))
  range)

(defun parse-int-range ()
  "Parses a int-range object."
  (parse-range (make-instance 'int-range) 'integer))

(defun parse-float-range ()
  "Parses a float-range object."
  (parse-range (make-instance 'float-range) 'double-float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collect-ranges (range-type range-parser-fn)
  "Parses and collects range objects into an array."
  (next-token)
  (loop
     with list-length = 0
     with range-list = '()
     for token = (string-left-trim *space-char-bag* (get-token))
     while (and token (char= (char token 0) #\()) do
       (handler-case 
           (progn
             (push (funcall range-parser-fn) range-list)
             (incf list-length)
             (next-token))
         (error () (emit-parse-error
                    "Error during range parsing!")))
     finally
       (progn
         (putback-token)
         (return (make-array list-length
                             :element-type range-type
                             :initial-contents (nreverse range-list))))))

(defun collect-int-ranges ()
  "Parses and collects int-range objects into an array."
  (collect-ranges 'int-range #'parse-int-range))

(defun collect-float-ranges ()
  "Parses and collects float-range objects into an array."
  (collect-ranges 'float-range #'parse-float-range))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod to-string ((object range))
  "Returns the string representation of a range object."
  (flet ((value-to-string (value)
           (case value
             (-inf "NEGINF")
             (+inf "POSINF")
             (unspec "UNSPEC")
             (t (assert (numberp value))
                (etypecase value
                  (integer (format nil "~d" value))
                  (float (format nil "~f" (coerce value 'single-float))))))))
    (with-output-to-string (s)
      (format s "(")
      (unless (= 1 (repeat-count object))
        (format s "~a " (repeat-count object)))
      (format s "~a ~a)"
              (value-to-string (min-value object))
              (value-to-string (max-value object))))))

