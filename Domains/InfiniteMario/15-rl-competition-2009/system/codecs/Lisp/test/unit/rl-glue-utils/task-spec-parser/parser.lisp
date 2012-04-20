
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
;;; $Revision: 411 $
;;; $Date: 2008-12-25 10:20:40 -0500 (Thu, 25 Dec 2008) $

(in-package #:org.rl-community.rl-glue-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:def-suite parser-suite
    :description "Tests for the general parser of task specification parser."
    :in task-spec-parser-suite)

(fiveam:in-suite parser-suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:test next-token
  "Test of token parsing."
  (let ((*parser* (make-parser :spec-string
                               (concatenate 'string
                                            "1st 2nd  3rd "
                                            (list #\Tab #\Space #\Newline)
                                            "  4th 5th .  6th,7th"))))
    (fiveam:is (string= "1st" (next-token)))
    (fiveam:is (string= "2nd" (next-token)))
    (fiveam:is (string= "3rd" (next-token)))
    (fiveam:is (string= "4th" (next-token)))
    (putback-token)
    (fiveam:is (string= "4th" (next-token)))
    (fiveam:is (string= "5th" (next-token ". ")))
    (fiveam:is (string= "6th" (next-token ",")))
    (fiveam:is (string= "7th" (next-token)))
    (fiveam:is (not (next-token)))
    (fiveam:is (not (next-token)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:test expectations
  "Test of parsing with expectation checking."
  (let ((*parser* (make-parser
                   :spec-string "Igy sem IGEN khm sokat ...")))
    (next-token)
    (expect-token "Igy")
    (fiveam:signals parser-error
      (expect-token "Error!"))
    (next-token)
    (expect-cond #'(lambda (token)
                     (string= token "sem"))
                 "I want 'sem'.")
    (fiveam:signals parser-error
      (expect-cond #'(lambda (token)
                       (numberp token))
                   "There is not a number."))
    (next-token)
    (handler-bind ((error #'(lambda (condition)
                              (declare (ignore condition))
                              (invoke-restart 'use-expected-token))))
      (expect-token "igen"))
    (fiveam:is (string= "igen" (get-token)))
    (next-token)
    (handler-bind ((error #'(lambda (condition)
                              (declare (ignore condition))
                              (invoke-restart 'ignore-found-token))))
      (expect-cond #'(lambda (token)
                       (string/= token "khm"))
                   "Word 'khm' is just a mistake.."))
    (fiveam:is (string= "sokat" (get-token)))
    (next-token)
    (handler-bind ((error #'(lambda (condition)
                              (declare (ignore condition))
                              (invoke-restart 'change-rest-of-spec
                                              "lat a pusztasagon."))))
      (expect-token "lat"))
    (fiveam:is (string= "lat" (get-token)))
    (next-token)
    (fiveam:is (string= "a" (get-token)))
    (next-token)
    (fiveam:is (string= "pusztasagon." (get-token)))
    (next-token)
    (fiveam:is (not (get-token)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:test number-conversion-basic
  "Test of parsing with number conversions and without min-max checkings."
  (let ((*parser* (make-parser
                   :spec-string "4 -4 +4 3.7 vua! 2 -2 5.25 +5.8 7.2d0 muu!")))
    ;; integer
    (next-token)
    (fiveam:is (= 4 (get-token-as-integer)))
    (next-token)
    (fiveam:is (= -4 (get-token-as-integer)))
    (next-token)
    (fiveam:is (integerp (get-token-as-integer)))
    (next-token)
    (fiveam:signals parser-error
      (get-token-as-integer))
    (next-token)
    (fiveam:signals parser-error
      (get-token-as-integer))
    ;; floating point
    (next-token)
    (fiveam:is (= 2.0d0 (get-token-as-float)))
    (next-token)
    (fiveam:is (= -2.0d0 (get-token-as-float)))
    (next-token)
    (fiveam:is (= 5.25d0 (get-token-as-float)))
    (next-token)
    (fiveam:is (typep (get-token-as-float) 'double-float))
    (next-token)
    (fiveam:is (= 7.2d0 (get-token-as-float)))
    (next-token)
    (fiveam:signals parser-error
      (get-token-as-float))
    (fiveam:is (not (next-token)))))

(fiveam:test number-conversion-min-max
  "Test of parsing with number conversion and with min-max checkings."
  (let ((*parser* (make-parser
                   :spec-string "8 12 -5 8.2d0 15.1d0 -46.3d0")))
    ;; integer
    (next-token)
    (fiveam:is (= 8 (get-token-as-integer :min 1 :max 10)))
    (next-token)
    (fiveam:signals parser-error
      (get-token-as-integer :min -4 :max 4))
    (next-token)
    (fiveam:signals parser-error
      (get-token-as-integer :min 0 :max 3))
    ;; floating point
    (next-token)
    (fiveam:is (= 8.2d0 (get-token-as-float :min 4.3 :max 11.2)))
    (next-token)
    (fiveam:signals parser-error
      (get-token-as-float :min 5.6 :max 12))
    (next-token)
    (fiveam:signals parser-error
      (get-token-as-float :min -11.8 :max 1.2))
    (fiveam:is (not (next-token)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:test collection
  "Test of parsing and collecting tokens."
  (let ((source-string "  one two three four "))
    (let ((token-list (parse-tokens-into-list source-string)))
      (fiveam:is (listp token-list))
      (fiveam:is (= 4 (length token-list)))
      (fiveam:is (string= "one" (pop token-list)))
      (fiveam:is (string= "two" (pop token-list)))
      (fiveam:is (string= "three" (pop token-list)))
      (fiveam:is (string= "four" (pop token-list)))
      (fiveam:is (null token-list)))))

