
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
;;; $Revision: 449 $
;;; $Date: 2009-01-25 17:09:19 -0500 (Sun, 25 Jan 2009) $

(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:rl-glue-mines-sarsa
  (:use #:common-lisp)
  (:export
   #:sarsa-agent
   #:mines
   #:experiment
   #:start-agent
   #:start-environment
   #:start-experiment))

