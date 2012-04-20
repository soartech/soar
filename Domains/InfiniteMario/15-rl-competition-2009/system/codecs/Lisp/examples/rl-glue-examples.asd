
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

(defpackage #:org.rl-community.rl-glue-examples-asdf
  (:use #:asdf #:common-lisp))

(in-package #:org.rl-community.rl-glue-examples-asdf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem rl-glue-examples
  :name "RL-Glue Common Lisp Codec Examples"
  :licence "Apache v2"
  :author "Gabor Balazs <gabalz@rl-community.org>"
  :maintainer "Gabor Balazs <gabalz@rl-community.org>"
  :description "Examples for RL-Glue components."
  :serial t
  :components
  ((:module skeleton
            :serial t
            :components
            ((:file "package")
             (:file "skeleton-agent")
             (:file "skeleton-environment")
             (:file "skeleton-experiment")))
   (:module mines-sarsa
            :serial t
            :components
            ((:file "package")
             (:file "sarsa-agent")
             (:file "mines")
             (:file "experiment"))))
  :depends-on ("rl-glue-codec" "rl-glue-utils"))

