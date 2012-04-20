
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
;;; $Revision: 746 $
;;; $Date: 2009-02-13 05:33:11 -0500 (Fri, 13 Feb 2009) $

(in-package #:cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:org.rl-community.rl-glue-utils-tests-asdf
  (:use #:asdf #:common-lisp))

(in-package #:org.rl-community.rl-glue-utils-tests-asdf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem rl-glue-utils-tests
  :name "RL-Glue Common Lisp Codec Utility Tests"
  :licence "Apache v2"
  :author "Gabor Balazs <gabalz@rl-community.org>"
  :maintainer "Gabor Balazs <gabalz@rl-community.org>"
  :description "Unit tests for the RL-Glue utilities."
  :serial t
  :components
  ((:file "main-suite")
   (:module "task-spec-parser"
            :serial t
            :components
            ((:file "task-spec-parser")
             (:file "parser")
             (:file "range"))))
  :depends-on ("fiveam" "rl-glue-utils"))

