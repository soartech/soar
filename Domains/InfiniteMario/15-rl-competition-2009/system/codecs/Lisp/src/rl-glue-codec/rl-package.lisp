
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
;;; $Revision: 761 $
;;; $Date: 2009-02-14 16:01:03 -0500 (Sat, 14 Feb 2009) $

(in-package #:cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:org.rl-community.rl-glue-codec
  (:nicknames #:rl-glue-codec)
  (:use #:common-lisp #:usocket)
  (:export
   #:get-codec-version
   #:get-vc-codec-version
   #:*init-buffer-size*
   ;; rl-common
   #:make-int-array
   #:int-array
   #:make-float-array
   #:float-array
   #:char-string
   #:observation
   #:make-observation
   #:action
   #:make-action
   #:rl-equalp
   ;; rl-client-agent
   #:agent
   #:agent-init
   #:agent-start
   #:agent-step
   #:agent-end
   #:agent-cleanup
   #:agent-message
   #:run-agent
   ;; rl-client-environment
   #:environment
   #:env-init
   #:env-start
   #:env-step
   #:env-cleanup
   #:env-message
   #:run-env
   ;; rl-client-experiment
   #:experiment
   #:rl-init
   #:rl-start
   #:rl-step
   #:rl-cleanup
   #:rl-close
   #:rl-return
   #:rl-num-steps
   #:rl-num-episodes
   #:rl-episode
   #:rl-agent-message
   #:rl-env-message))

(in-package #:org.rl-community.rl-glue-codec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *optimize-settings*
  '(optimize
    (speed 3)
    (safety 0)
    (space 0)
    (debug 0)
    (compilation-speed 0)
    #+lispworks
    (float 0))
  "General optimization settings of the package.")

