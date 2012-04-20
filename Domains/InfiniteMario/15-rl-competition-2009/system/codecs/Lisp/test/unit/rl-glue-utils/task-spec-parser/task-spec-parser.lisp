
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

(fiveam:def-suite task-spec-parser-suite
    :description "Tests for the task specification parser."
    :in main-suite)

(fiveam:in-suite task-spec-parser-suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:test parse-task-spec
  "Parsing example task specifications."
  (labels
      ((check-array (got-array expected-list)
         "Checking the expectations about ranges in a container."
         (fiveam:is (= (length expected-list) (length got-array)))
         (loop
            for e in expected-list
            for g across got-array do
              (fiveam:is (equalp (repeat-count g) (first e)))
              (fiveam:is (equalp (min-value g) (second e)))
              (fiveam:is (equalp (max-value g) (third e)))))
       (check-task-spec (task-spec
                         version
                         problem-type
                         discount-factor
                         int-observations
                         float-observations
                         char-observations
                         int-actions
                         float-actions
                         char-actions
                         rewards
                         extra-spec)
         "Checking the expectations about a task specification."
         (fiveam:is (string= (version task-spec) version))
         (fiveam:is (string= (problem-type task-spec) problem-type))
         (fiveam:is (= (discount-factor task-spec) discount-factor))
         (check-array (int-observations task-spec)
                      int-observations)
         (check-array (float-observations task-spec)
                      float-observations)
         (fiveam:is (= (char-observations task-spec)
                       char-observations))
         (check-array (int-actions task-spec)
                      int-actions)
         (check-array (float-actions task-spec)
                      float-actions)
         (fiveam:is (= (char-actions task-spec)
                       char-actions))
         (fiveam:is (= (repeat-count (rewards task-spec)) 1))
         (fiveam:is (equalp (min-value (rewards task-spec)) (first rewards)))
         (fiveam:is (equalp (max-value (rewards task-spec)) (second rewards)))
         (fiveam:is (string= (extra-spec task-spec) extra-spec))))
    (check-task-spec
     (parse-task-spec
      (concatenate 'string
                   "VERSION RL-Glue-3.0 PROBLEMTYPE episodic "
                   "DISCOUNTFACTOR 1 OBSERVATIONS INTS (3 0 1) DOUBLES "
                   "(2 -1.2 0.5) (-.07 .07) CHARCOUNT 1024 ACTIONS INTS "
                   "(0 4) REWARDS (-5.0 5.0) EXTRA "
                   "some other stuff goes here"))
     "RL-Glue-3.0" "episodic" 1
     (list '(3 0 1)) (list '(2 -1.2d0 0.5d0) '(1 -0.07d0 0.07d0)) 1024
     (list '(1 0 4)) (list) 0
     '(-5.0d0 5.0d0) "some other stuff goes here")
    (check-task-spec
     (parse-task-spec
      (concatenate 'string
                   "VERSION RL-Glue-3.0 PROBLEMTYPE episodic "
                   "DISCOUNTFACTOR 1 OBSERVATIONS INTS (UNSPEC 1) "
                   "ACTIONS DOUBLES (NEGINF POSINF) CHARCOUNT 0 "
                   "REWARDS (UNSPEC UNSPEC) EXTRA Name: Test Problem A"))
     "RL-Glue-3.0" "episodic" 1
     (list '(1 unspec 1)) (list) 0
     (list) (list '(1 -inf +inf)) 0
     '(unspec unspec) "Name: Test Problem A")
    (check-task-spec
     (parse-task-spec
      (concatenate 'string
                   "VERSION RL-Glue-3.0 PROBLEMTYPE episodic "
                   "DISCOUNTFACTOR 1 OBSERVATIONS DOUBLES "
                   "(-1.2 0.5) (-.07 .07) ACTIONS INTS (0 2) "
                   "REWARDS (-1 0) EXTRA Name=Traditional-"
                   "Mountain-Car Cutoff=None Random-Starts=True"))
     "RL-Glue-3.0" "episodic" 1
     (list) (list '(1 -1.2d0 0.5d0) '(1 -0.07d0 0.07d0)) 0
     (list '(1 0 2)) (list) 0
     '(-1 0) (concatenate 'string
                          "Name=Traditional-Mountain-Car "
                          "Cutoff=None Random-Starts=True"))
    (fiveam:signals parser-error
      (parse-task-spec
       "VERSION Real-Time-Strategy-1.0 anything what you want"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:test to-string
  "Test of task-spec string conversion."
  (flet ((check-with-spec (spec-string)
           (fiveam:is (string= spec-string
                               (to-string (parse-task-spec spec-string))))))
    (check-with-spec
      (concatenate 'string
                   "VERSION RL-Glue-3.0 PROBLEMTYPE episodic "
                   "DISCOUNTFACTOR 1.0 OBSERVATIONS INTS (3 0 1) DOUBLES "
                   "(2 -1.2 0.5) (-0.07 0.07) CHARCOUNT 1024 ACTIONS "
                   "INTS (0 4) REWARDS (-5.0 5.0) EXTRA "
                   "some other stuff goes here"))
    (check-with-spec
      (concatenate 'string
                   "VERSION RL-Glue-3.0 PROBLEMTYPE episodic "
                   "DISCOUNTFACTOR 0.7 OBSERVATIONS INTS (UNSPEC 1) "
                   "ACTIONS DOUBLES (NEGINF POSINF) CHARCOUNT 2 "
                   "REWARDS (UNSPEC UNSPEC) EXTRA Name: Test Problem A"))
    (check-with-spec
      (concatenate 'string
                   "VERSION RL-Glue-3.0 PROBLEMTYPE episodic "
                   "DISCOUNTFACTOR 0.2 OBSERVATIONS DOUBLES "
                   "(-1.2 0.5) (0 -0.07 0.07) ACTIONS INTS (0 2) "
                   "REWARDS (-1.0 0.0) EXTRA Name=Traditional-"
                   "Mountain-Car Cutoff=None Random-Starts=True"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:test range-helpers
  "Test of the range helper task specification related functions."
  (let ((ranges (make-array 3
                            :initial-contents
                            (list (make-instance 'int-range
                                                 :repeat-count 1
                                                 :min-value 3
                                                 :max-value 5)
                                  (make-instance 'int-range
                                                 :repeat-count 0
                                                 :min-value -4
                                                 :max-value 2)
                                  (make-instance 'float-range
                                                 :repeat-count 3
                                                 :min-value 0.5d0
                                                 :max-value 1.0d0)))))
    (fiveam:is (equalp '(8 1.5d0 1.5d0 1.5d0)
                       (across-ranges #'(lambda (min max)
                                          (+ min max))
                                      ranges)))
    (fiveam:is (= 4 (ranges-dimension ranges)))))

