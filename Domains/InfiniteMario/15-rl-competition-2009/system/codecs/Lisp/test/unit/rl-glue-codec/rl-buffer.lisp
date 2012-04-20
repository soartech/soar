
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
;;; $Revision: 457 $
;;; $Date: 2009-01-27 05:02:48 -0500 (Tue, 27 Jan 2009) $

(in-package #:org.rl-community.rl-glue-codec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fiveam:def-suite buffer-suite
    :description "Tests for buffer handling."
    :in main-suite)

(fiveam:in-suite buffer-suite)

(defparameter *test-char-code-limit* 256)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((*random-state* (make-random-state t)))
  (fiveam:test char-encode/decode
    "Test of character encode/decode operations."
    (fiveam:for-all ((ch (fiveam:gen-character :code-limit
                                               *test-char-code-limit*)))
      (fiveam:is (char= ch (char-decoder (char-encoder ch))))))
  (fiveam:test integer-encode/decode
    "Test of integer encode/decode operations."
    (fiveam:for-all ((int (gen-32-integer)))
      (fiveam:is (= int (integer-decoder (integer-encoder int))))))
  (fiveam:test float-encode/decode
    "Test of float encode/decode operations."
    (fiveam:for-all ((fl (fiveam:gen-float :type 'double-float)))
      (fiveam:is (= fl (multiple-value-call #'float-decoder
                         (float-encoder fl)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((*random-state* (make-random-state t))
      (*init-buffer-size* 4))
  (fiveam:test basic-read/write
    "Test of basic read/write operations on buffers."
    (let ((buffer (make-buffer)))
      (fiveam:is (= *init-buffer-size*
                    (length (buffer-bytes buffer))))
      ;; dirty cases
      (fiveam:signals empty-buffer-error
        (buffer-read-char buffer))
      (fiveam:signals empty-buffer-error
        (buffer-read-int buffer))
      (fiveam:signals empty-buffer-error
        (buffer-read-float buffer))
      (fiveam:is (eq 'any-value (handler-bind
                                    ((error #'(lambda (condition)
                                                (declare (ignore condition))
                                                (invoke-restart 'use-value
                                                                'any-value))))
                                  (buffer-read-float buffer))))
      ;; clean cases
      (fiveam:for-all ((ch (fiveam:gen-character :code-limit
                                                 *test-char-code-limit*))
                       (int (gen-32-integer))
                       (fl (fiveam:gen-float :type 'double-float)))
        (buffer-clear buffer)
        (buffer-write-char ch buffer)
        (buffer-write-int int buffer)
        (buffer-write-float fl buffer)
        (fiveam:is (= (buffer-size buffer)
                      (+ +bytes-per-char+
                         +bytes-per-integer+
                         +bytes-per-float+)))
        (setf (buffer-offset buffer) 0)
        (fiveam:is (char= ch (buffer-read-char buffer)))
        (fiveam:is (= int (buffer-read-int buffer)))
        (fiveam:is (= fl (buffer-read-float buffer)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((*random-state* (make-random-state t))
      (*init-buffer-size* 8))
  (fiveam:test sequence-read/write
    "Test of sequence read/write operations on buffers."
    (let ((buffer (make-buffer)))
      (fiveam:for-all ((str (fiveam:gen-string
                             :length (fiveam:gen-integer
                                      :min 0 :max 1024)
                             :elements (fiveam:gen-character
                                        :code-limit *test-char-code-limit*)))
                       (int (fiveam:gen-list
                             :length (fiveam:gen-integer
                                      :min 0 :max 1024)
                             :elements (gen-32-integer)))
                       (flt (fiveam:gen-list
                             :length (fiveam:gen-integer
                                      :min 0 :max 1024)
                             :elements (fiveam:gen-float
                                        :type 'double-float))))
        (let ((aint (make-int-array (length int) :initial-contents int))
              (aflt (make-float-array (length flt) :initial-contents flt)))
          (buffer-clear buffer)
          (buffer-write-string str buffer)
          (buffer-write-int-seq aint buffer)
          (buffer-write-float-seq aflt buffer)
          (setf (buffer-offset buffer) 0)
          (fiveam:is (string= str (buffer-read-string buffer)))
          (fiveam:is (equalp aint (buffer-read-int-seq buffer)))
          (fiveam:is (equalp aflt (buffer-read-float-seq buffer))))))))

