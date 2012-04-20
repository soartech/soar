
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

(in-package #:cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:org.rl-community.rl-glue-tests
  (:nicknames #:rl-glue-tests)
  (:use #:common-lisp #:rl-glue-codec #:rl-glue-utils)
  (:export
   ;; test-1
   #:start-test-1-agent
   #:start-test-1-environment
   #:start-test-1-experiment
   ;; test-empty
   #:start-test-empty-agent
   #:start-test-empty-environment
   #:start-test-empty-experiment
   ;; test-init-cleanup
   #:start-test-init-cleanup-experiment
   ;; test-message
   #:start-test-message-agent
   #:start-test-message-environment
   #:start-test-message-experiment
   ;; test-rl-episode
   #:start-test-rl-episode-experiment
   ;; test-sanity
   #:start-test-sanity-experiment
   ;; test-seeds
   #:start-test-seeds-environment
   #:start-test-seeds-experiment
   ;; test-speed
   #:start-test-speed-environment
   #:start-test-speed-experiment))

