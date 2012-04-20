
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
;;; $Revision: 430 $
;;; $Date: 2009-01-21 16:19:07 -0500 (Wed, 21 Jan 2009) $

(in-package #:rl-glue-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *parser* nil
  "The currently used parser.")

(defstruct parser
  "Parser object used for tokenize strings."
  (token "" :type (or boolean string))
  (need-new-p t :type boolean)
  (spec-string "" :type string)
  (spec (make-instance 'task-spec) :type task-spec))

(defmacro get-token ()
  "Returns the last parsed token."
  `(parser-token *parser*))

(defmacro get-spec-string ()
  "Returns the specification string."
  `(parser-spec-string *parser*))

(defmacro get-spec ()
  "Returns the task specification."
  `(parser-spec *parser*))

(defparameter *space-char-bag* '(#\Space #\Tab #\Newline)
  "List of space characters.")

(defun next-token (&optional (char-bag *space-char-bag*))
  "Parses the next token delimited by the CHAR-BAG characters.
 The beginning whitespaces are always ignored. NIL token means
 that there no more token left to parse."
  (if (parser-need-new-p *parser*)
      (let* ((spec-string (string-left-trim *space-char-bag*
                                            (get-spec-string)))
             (spec-string-length (length spec-string)))
        (if (plusp spec-string-length)
            (let* ((pos (or (position-if #'(lambda (c)
                                             (find c char-bag)) spec-string)
                            spec-string-length)))
              (setf (get-token) (subseq spec-string 0 pos))
              (setf (get-spec-string) (string-left-trim char-bag
                                                        (subseq spec-string
                                                                pos))))
            (setf (get-token) nil)))
      (setf (parser-need-new-p *parser*) t))
  (get-token))

(defun putback-token ()
  "Virtually puts back the token to the beginning of the source string,
 so the next-token function will return it on its next call."
  (assert (parser-need-new-p *parser*))
  (setf (parser-need-new-p *parser*) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition parser-error (error)
  ((expected
    :reader expected
    :initarg :expected
    :documentation "The expected token during parsing.")
   (found
    :reader found
    :initarg :found
    :documentation "The found token during parsing.")
   (spec-string
    :reader spec-string
    :initarg :spec-string
    :documentation "The unparsed part of the source string."))
  (:documentation "Parse error.")
  (:report (lambda (condition stream)
             (format stream
                     "Parse error (expected:'~a',found:'~a',spec-tail:'~a')!"
                     (expected condition)
                     (found condition)
                     (spec-string condition)))))

(defun emit-parse-error (description
                         &key
                         expected
                         (delim-char-bag *space-char-bag*))
  "Emits a restartable parse error."
  (restart-case (error 'parser-error
                       :expected description
                       :found (get-token)
                       :spec-string (get-spec-string))
    (use-expected-token ()
      :report "Use the expected token instead of the found one."
      :test (lambda (c) (declare (ignore c)) (when expected t))
      (setf (get-token) expected))
    (ignore-found-token ()
      :report "Ignore the found token and get the next one."
      (next-token delim-char-bag))
    (change-rest-of-spec (new-spec-tail)
      :report "Ignore the found token and change the unparsed tail
 of the specification string."
      :interactive (lambda ()
                     (format t "New string source tail: ")
                     (multiple-value-list (eval (read))))
      (setf (get-spec-string) new-spec-tail)
      (next-token delim-char-bag))))

(defun expect-cond (condition-fn description)
  "Checks whether the CONDITION-FN returns T,
 and emits a restartable parse error if not."
  (loop until (funcall condition-fn (get-token)) do
       (emit-parse-error description)))

(defun expect-token (expected-token)
  "Checks whether the EXPECTED-TOKEN and the FOUND tokens are equal,
 and emits a restartable parse error if not."
  (loop until (string= expected-token (get-token))
     do (emit-parse-error expected-token :expected expected-token)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-token-as-number (type-name typep-fn min max)
  "Returns the next token as a number."
  (let ((*read-default-float-format* 'double-float)
        (object nil))
    (expect-cond #'(lambda (token)
                     (setf object (read-from-string token nil))
                     (and (funcall typep-fn object)
                          (or (not min) (<= min object))
                          (or (not max) (>= max object))))
                 (format nil
                         "~a [~a,~a]"
                         type-name
                         (or min '-inf)
                         (or max '+inf)))
    object))

(defun get-token-as-integer (&key min max)
  "Returns the next token as an integer number."
  (get-token-as-number "integer" #'integerp min max))

(defun get-token-as-float (&key min max)
  "Returns the next token as a floating point number."
  (coerce (get-token-as-number "number" #'numberp min max) 'double-float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-tokens-into-list (source-string)
  "Parses all the tokens into a list."
  (let ((*parser* (make-parser :spec-string source-string)))
    (loop
       for token = (next-token)
       while token
       collect token)))

