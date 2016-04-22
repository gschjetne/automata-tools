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
   (transitions :reader get-transition-table
                :initarg :transitions)))

(defgeneric accepting-p (automaton))

(defmethod accepting-p ((a automaton))
  (intersection (get-current-states a)
                (get-accepting-states a)))

(defgeneric reset-automaton (automaton))

(defmethod reset-automaton ((a automaton))
  (setf (get-current-states a) (list (get-initial-state a))))

(defun compile-delta (a)
  (let ((alphabet (get-alphabet a))
        (transitions (get-transition-table a)))
    (dolist (state (get-states a))
      (dolist (symbol alphabet)
        (eval
         `(defmethod delta ((a (eql ,a)) (state (eql (quote ,state)))
                            (symbol (eql (quote ,symbol))))
            (quote ,(reduce #'append
                            (mapcar #'cdr
                                    (remove-if-not (lambda (c) (eql c symbol))
                                                   (cdr (find state
                                                              transitions
                                                              :key #'car))
                                                   :key #'car))))))))))

(defmethod initialize-instance :after ((a automaton) &key)
  (reset-automaton a)
  (compile-delta a))

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

(defmethod graph-object-node ((a automaton) state)
  (make-instance 'node
                 :attributes `(:label ,(write-to-string state :escape nil)
                               :shape ,(if (find state
                                                 (get-accepting-states a))
                                           :doubleoctagon
                                           :circle)
                               :style :filled
                               :fillcolor ,(if (find state
                                                     (get-current-states a))
                                               "#ffff00"
                                               "#ffffff"))))

(defmethod graph-object-points-to ((a automaton) state)
  (flet ((symbol-to-graph-edge (state symbol)
           (make-instance 'attributed
                 :object state
                 :attributes `(:label ,(write-to-string symbol)))))
    (loop for mapping in (cdr (find state (get-transition-table a) :key #'car))
          append (loop with symbol = (car mapping)
                       for destination in (cdr mapping)
                       collect (symbol-to-graph-edge destination symbol)))))

(defun states-in-table (transition-table)
  (remove-duplicates
   (loop for row in transition-table
         append (cons (car row)
                      (reduce #'append
                              (mapcar #'cdr (cdr row)))))))

(defun make-automaton (type transition-table initial-state accepting-states
                       &key additional-states)
  (make-instance type
                 :states (union (states-in-table transition-table)
                                additional-states)
                 :transitions transition-table
                 :initial-state initial-state
                 :accepting-states accepting-states))

(defgeneric get-alphabet (automaton))

(defmethod get-alphabet ((a automaton))
  (remove-duplicates
   (mapcar #'car
           (reduce #'append
                   (mapcar #'cdr
                           (get-transition-table a))))))

(defgeneric delta (automaton state symbol))

(defgeneric extended-delta (automaton state string))

(defmethod extended-delta ((a automaton) state (string sequence))
  (extended-delta a state (coerce string 'list)))

(defgeneric step-automaton (automaton string-or-symbol))

(defmethod step-automaton ((a automaton) s)
  (typecase s
    (sequence (setf (get-current-states a) (extended-delta a (get-current-states a) s)))
    (t (step-automaton a (list s)))))
