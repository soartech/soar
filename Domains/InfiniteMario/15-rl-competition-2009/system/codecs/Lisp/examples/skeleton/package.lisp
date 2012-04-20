
;;; Copyright 2009 Gabor Balazs
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
;;; $Revision: 720 $
;;; $Date: 2009-02-10 09:32:33 -0500 (Tue, 10 Feb 2009) $

(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:rl-glue-skeleton
  (:use #:common-lisp)
  (:export
   #:skeleton-agent
   #:skeleton-environment
   #:skeleton-experiment
   #:start-agent
   #:start-environment
   #:start-experiment))

