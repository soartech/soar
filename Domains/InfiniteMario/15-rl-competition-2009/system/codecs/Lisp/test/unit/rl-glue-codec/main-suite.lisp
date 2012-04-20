
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

(fiveam:def-suite main-suite
    :description "Main test suite for RL-Glue codec tests.")

(fiveam:in-suite main-suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gen-32-integer ()
  "Generates an integer representable in 32 bit dword."
  (fiveam:gen-integer :min (- rl-glue-codec::+uint-minneg+)
                      :max (1- rl-glue-codec::+uint-minneg+)))

