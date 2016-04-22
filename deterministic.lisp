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

;;;; deterministic.lisp

(in-package #:automata-tools)

(defclass dfa (automaton) ())

(defmethod deterministic-p ((a dfa)) t)

(defmethod initialize-instance :after ((a dfa) &key)
  (unless (deterministic-p (get-transition-table a))
    (warn "Non-deterministic transition table for a deterministic automaton")))

(defmethod extended-delta ((a dfa) state (string list))
  (typecase state
    (list (if string
              (extended-delta a (car (delta a (car state) (car string))) (cdr string))
              state))
    (t (extended-delta a (list state) string))))
