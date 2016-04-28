;; Automata Tools for Common Lisp
;; Copyright (C) 2016 Grim Schjetne
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; automata-tools.lisp

(in-package #:automata-tools)

(defclass automaton ()
  ((current-states :accessor get-current-states)
   (states :reader get-states
           :initarg :states)
   (initial-state :reader get-initial-state
                  :initarg :initial-state)
   (accepting-states :reader get-accepting-states
                     :initarg :accepting-states)
   (alphabet :reader get-alphabet
             :initarg :alphabet)
   (delta :reader get-delta-function
          :initarg :delta)))

(defmethod initialize-instance :after ((a automaton) &key)
  (reset-automaton a))

(defgeneric accepting-p (automaton))

(defmethod accepting-p ((a automaton))
  (intersection (get-current-states a)
                (get-accepting-states a)))

(defgeneric reset-automaton (automaton))

(defmethod reset-automaton ((a automaton))
  (setf (get-current-states a) (list (get-initial-state a))))

(defgeneric deterministic-p (automaton-or-transitions))

(defmethod deterministic-p ((transitions list))
  (labels ((det-p (state)
             (if state
                 (and (= 2 (length (car state)))
                      (det-p (cdr state)))
                 t)))
    (if transitions
        (and (det-p (cdar transitions))
             (deterministic-p (cdr transitions)))
        t)))

(defun states-in-table (transition-table)
  (remove-if-not #'identity
   (remove-duplicates
    (loop for row in transition-table
          append (cons (car row)
                       (reduce #'append
                               (mapcar #'cdr (cdr row))))))))

(defun find-transitions (transitions state symbol)
  (reduce #'append
          (mapcar #'cdr
                  (remove-if-not (lambda (c) (eql c symbol))
                                 (cdr (find state
                                            transitions
                                            :key #'car))
                                 :key #'car))))

(defun compile-transitions (states alphabet transitions)
  (eval
   `(lambda (state symbol)
      (case state
        ,@(loop for state in states
                collect
                `(,state (case symbol
                           ,@(loop for symbol in alphabet
                                   collect `(,symbol
                                             (quote ,(find-transitions
                                                      transitions state symbol))))
                           (otherwise nil))))
        (otherwise nil)))))

(defun make-automaton (type transitions initial-state accepting-states
                       &key additional-states)
  (let ((alphabet (sort (remove-duplicates
                         (mapcar #'car
                                  (reduce #'append
                                          (mapcar #'cdr
                                                  transitions))))
                        #'multi-lessp))
        (states (states-in-table transitions)))
    (make-instance type
                   :states (sort (union states additional-states)
                                 #'multi-lessp)
                   :initial-state initial-state
                   :accepting-states accepting-states
                   :alphabet alphabet
                   :delta (compile-transitions states alphabet transitions))))

(defgeneric delta (automaton state symbol))

(defmethod delta ((a automaton) state symbol)
  (funcall (get-delta-function a) state symbol))

(defgeneric extended-delta (automaton state string))

(defmethod extended-delta ((a automaton) state (string sequence))
  (extended-delta a state (coerce string 'list)))

(defgeneric step-automaton (automaton string-or-symbol))

(defmethod step-automaton ((a automaton) s)
  (typecase s
    (sequence (setf (get-current-states a) (extended-delta a (get-current-states a) s)))
    (t (step-automaton a (list s)))))
