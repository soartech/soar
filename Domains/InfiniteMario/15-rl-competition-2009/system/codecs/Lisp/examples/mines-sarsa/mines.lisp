
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
;;; $Revision: 588 $
;;; $Date: 2009-02-04 16:20:28 -0500 (Wed, 04 Feb 2009) $

(in-package #:rl-glue-mines-sarsa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +world-free+ 0)
(defconstant +world-obstacle+ 1)
(defconstant +world-mine+ 2)
(defconstant +world-goal+ 3)

(defclass mines (rl-glue-codec:environment)
  ((rand-state
    :accessor rand-state
    :initform (make-random-state t)
    :documentation "Random state of the environment.")
   (step-num
    :accessor step-num
    :initform 0
    :documentation "Step number.")
   (field-map
    :accessor field-map
    :documentation "Map of the mine field.")
   (start-row
    :accessor start-row
    :initform 1
    :documentation "Row of starting position.")
   (start-col
    :accessor start-col
    :initform 1
    :documentation "Column of starting position.")
   (fixed-start-state
    :accessor fixed-start-state
    :initform nil
    :documentation "T if the start state is given by the user,
 NIL if it is generated automatically by the environment itself.")
   (agent-row
    :accessor agent-row
    :documentation "Row of agent's current position.")
   (agent-col
    :accessor agent-col
    :documentation "Column of agent's current position.")
   (terminal
    :accessor terminal
    :documentation "Shows whether the agent is in terminal state."))
  (:documentation "Environment about passing through a mine field."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun num-rows (mines)
  "Returns the number of rows of the field map of MINES."
  (array-dimension (field-map mines) 0))

(defun num-cols (mines)
  "Returns the number of columns of the field map of MINES."
  (array-dimension (field-map mines) 1))

(defun make-observation (mines row col)
  "Creates an observation of a mines environment state."
  (rl-glue-codec:make-observation
   :int-array (rl-glue-codec:make-int-array
               1
               :initial-contents
               (list (+ (* col (num-rows mines)) row)))))

(defun pos-type (mines row col)
  "Returns the land type of the specified position."
  (aref (field-map mines) row col))

(defun valid-position-p (mines row col)
  "Checks whether ROW and COL specifies a valid position in MINES."
  (and (<= 0 row) (< row (num-rows mines))
       (<= 0 col) (< col (num-cols mines))
       (/= (pos-type mines row col) +world-obstacle+)))

(defun update-position (mines action-idx)
  "Updates the agent's position according to ACTION-IDX."
  (with-accessors ((arow agent-row) (acol agent-col)) mines
    (let ((new-row arow) (new-col acol))
      (ecase action-idx
        (0 (decf new-col))
        (1 (incf new-col))
        (2 (decf new-row))
        (3 (incf new-row)))
      (when (valid-position-p mines new-row new-col)
        (setf arow new-row)
        (setf acol new-col))))
  mines)

(defun get-reward (mines)
  "Returns the (reward,terminal) pair for the current position."
  (let ((postype (pos-type mines (agent-row mines) (agent-col mines))))
    (cond ((= postype +world-goal+) (values 10.0d0 t))
          ((= postype +world-mine+) (values -100.0d0 t))
          (t (values -1.0d0 nil)))))

(defun set-random-start-state (mines)
  "Sets a random start state for the agent."
  (with-accessors ((rand-state rand-state) (map field-map)
                   (start-row start-row) (start-col start-col)) mines
    (loop
       with row = (num-rows mines)
       with col = (num-cols mines)
       for r = (random row rand-state)
       for c = (random col rand-state)
       while (/= +world-free+ (aref map r c))
       finally
         (setf start-row r)
         (setf start-col c))
    (setf (agent-row mines) start-row)
    (setf (agent-col mines) start-col)))

(defun print-state (mines)
  "Prints the current state of MINES."
  (with-accessors ((agent-row agent-row) (agent-col agent-col)
                   (map field-map)) mines
    (format t "Agent is at: ~d,~d~%" agent-row agent-col)
    (format t "Columns:0-10                10-17~%")
    (format t "Col   ")
    (dotimes (col (num-cols mines))
      (format t " ~d" (mod col 10)))
    (format t "~%")
    (dotimes (row (num-rows mines))
      (format t "Row: ~d" row)
      (dotimes (col (num-cols mines))
        (format t " ~a"
                (cond
                  ((and (= agent-row row) (= agent-col col)) #\A)
                  ((= (aref map row col) +world-goal+) #\G)
                  ((= (aref map row col) +world-mine+) #\M)
                  ((= (aref map row col) +world-obstacle+) #\*)
                  ((= (aref map row col) +world-free+) #\Space)
                  (t (error "Unknown state!")))))
      (format t "~%"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rl-glue-codec:env-init ((env mines))
  (let ((row 6) (col 18))
    (setf (field-map env)
          (make-array (list row col)
                      :element-type 'integer
                      :initial-contents
                      '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                        (1 0 0 0 0 0 0 2 2 0 0 0 0 0 0 0 0 1)
                        (1 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 1)
                        (1 0 0 0 0 0 0 0 0 2 2 2 0 0 0 0 1 1)
                        (1 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 1)
                        (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))
    (format nil
            (concatenate
             'string
             "VERSION RL-Glue-3.0 PROBLEMTYPE episodic DISCOUNTFACTOR 1 "
             "OBSERVATIONS INTS (0 ~a) ACTIONS INTS (0 ~a) "
             "REWARDS (-100.0 10.0) EXTRA mines by Gabor Balazs")
            (1- (* row col)) 3)))

(defmethod rl-glue-codec:env-start ((env mines))
  (with-accessors ((start-row start-row) (start-col start-col)) env
    (setf (agent-row env) start-row)
    (setf (agent-col env) start-col)
    (unless (and (fixed-start-state env)
                 (valid-position-p env start-row start-col))
      (set-random-start-state env))
    (assert (valid-position-p env start-row start-col))
    (setf (terminal env) nil)
    (make-observation env start-row start-col)))

(defmethod rl-glue-codec:env-step ((env mines) action)
  (incf (step-num env))
  (update-position env (aref (rl-glue-codec:int-array action) 0))
  (multiple-value-bind (reward terminal) (get-reward env)
    (values reward
            (make-observation env (agent-row env) (agent-col env))
            terminal)))

(defmethod rl-glue-codec:env-cleanup ((env mines))
  (setf (field-map env) nil)
  env)

(defmethod rl-glue-codec:env-message ((env mines) input-message)
  (cond
    ((string= input-message "set-random-start-state")
     (setf (fixed-start-state env) nil)
     "Message understood. Using random start state.")
    ((and (>= (length input-message) 16)
              (string= (subseq input-message 0 15) "set-start-state"))
     (with-input-from-string (s input-message :start 15)
       (setf (start-row env) (read s))
       (setf (start-col env) (read s)))
     (setf (fixed-start-state env) t)
     "Message understood. Using fixed start state.")
    ((string= input-message "print-state")
     (print-state env)
     "Message understood. Printed the state.")
    (t "Mines environment does not respond to that message.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-environment (&rest args)
  "Starts a mines environment."
  (apply #'rl-glue-codec:run-env (make-instance 'mines) args))

