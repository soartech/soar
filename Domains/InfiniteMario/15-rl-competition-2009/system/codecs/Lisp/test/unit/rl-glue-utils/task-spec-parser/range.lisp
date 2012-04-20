
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

(fiveam:def-suite range-suite
    :description "Tests for the range class of task specification."
    :in task-spec-parser-suite)

(fiveam:in-suite range-suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:test parse-range
  "Test of parsing a range."
  (let ((*parser* (make-parser :spec-string " (3 7)  ")))
    (next-token ")")
    (let ((range (parse-int-range)))
      (fiveam:is (= 1 (repeat-count range)))
      (fiveam:is (= 3 (min-value range)))
      (fiveam:is (= 7 (max-value range)))))
  (let ((*parser* (make-parser :spec-string "(4.3 7.1)")))
    (next-token ")")
    (let ((range (parse-float-range)))
      (fiveam:is (= 1 (repeat-count range)))
      (fiveam:is (= 4.3d0 (min-value range)))
      (fiveam:is (= 7.1d0 (max-value range)))))
  (let ((*parser* (make-parser :spec-string "(3 NEGINF POSINF)")))
    (next-token ")")
    (let ((range (parse-float-range)))
      (fiveam:is (= 3 (repeat-count range)))
      (fiveam:is (eql '-inf (min-value range)))
      (fiveam:is (eql '+inf (max-value range)))))
  (let ((*parser* (make-parser :spec-string "(UNSPEC UNSPEC)")))
    (next-token ")")
    (let ((range (parse-int-range)))
      (fiveam:is (= 1 (repeat-count range)))
      (fiveam:is (eql 'unspec (min-value range)))
      (fiveam:is (eql 'unspec (max-value range)))))
  (let ((*parser* (make-parser :spec-string "(1.2 4 5)")))
    (next-token ")")
    (fiveam:signals error
      (parse-int-range)))
  (let ((*parser* (make-parser :spec-string "(4 5.6 3)")))
    (next-token ")")
    (fiveam:signals error
      (parse-int-range)))
  (let ((*parser* (make-parser :spec-string "(6 a)")))
    (next-token ")")
    (fiveam:signals error
      (parse-float-range))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:test collect-ranges
  "Test of collecting ranges."
  (flet ((check-range (range count min max)
           (fiveam:is (= count (repeat-count range)))
           (fiveam:is (= min (min-value range)))
           (fiveam:is (= max (max-value range)))))
    ;; int case
    (let ((*parser* (make-parser
                     :spec-string " (2 41 5) (4 5)(2 5) ( 3 7 2 ) (1 2)")))
      (let ((ranges (collect-int-ranges)))
        (fiveam:is (= 5 (length ranges)))
        (check-range (aref ranges 0) 2 41 5)
        (check-range (aref ranges 1) 1 4 5)
        (check-range (aref ranges 2) 1 2 5)
        (check-range (aref ranges 3) 3 7 2)
        (check-range (aref ranges 4) 1 1 2)))
    ;; float case
    (let ((*parser* (make-parser
                     :spec-string " (4.3 5.6) (3 4.3 25.9)(2 9.3 12.3) (E) ")))
      (handler-bind ((parser-error #'(lambda (condition)
                                       (declare (ignore condition))
                                       (invoke-restart 'change-rest-of-spec
                                                       "(4 5.0 7.6)"))))
        (let ((ranges (collect-float-ranges)))
          (fiveam:is (= 4 (length ranges)))
          (check-range (aref ranges 0) 1 4.3d0 5.6d0)
          (check-range (aref ranges 1) 3 4.3d0 25.9d0)
          (check-range (aref ranges 2) 2 9.3d0 12.3d0)
          (check-range (aref ranges 3) 4 5.0d0 7.6d0))))))

