
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
;;; $Revision: 734 $
;;; $Date: 2009-02-11 15:28:13 -0500 (Wed, 11 Feb 2009) $

(in-package #:cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:org.rl-community.rl-glue-utils
  (:nicknames #:rl-glue-utils)
  (:use #:common-lisp #:rl-glue-codec)
  (:export
   ;; generic
   #:to-string
   ;; task-spec-parser
   #:range
   #:int-range
   #:make-int-range
   #:float-range
   #:make-float-range
   #:repeat-count
   #:min-value
   #:max-value
   #:make-int-range-array
   #:make-float-range-array
   #:task-spec
   #:make-task-spec
   #:version
   #:problem-type
   #:discount-factor
   #:int-observations
   #:float-observations
   #:char-observations
   #:int-actions
   #:float-actions
   #:char-actions
   #:rewards
   #:extra-spec
   #:parse-task-spec
   #:across-ranges
   #:ranges-dimension))

