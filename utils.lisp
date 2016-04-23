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

;;;; utils.lisp

(in-package #:automata-tools)

(defun multi-lessp (x y)
  (string-lessp (write-to-string x :escape nil)
                (write-to-string y :escape nil)))

(defun cartesian-cons (l1 l2)
  (loop for x in l1
        append (loop for y in l2 collect (cons x y))))

;; Todo: Remove combine-states in favour of the more general combined-state
(defun combine-states (s1 s2)
  (when s1
    (when s2
      (intern (format nil "~A,~A" s1 s2)))))

(defun combined-state (set)
  (if set
      (intern (format nil "~{~A~^,~}" (sort (remove-duplicates set) #'multi-lessp)))
      nil))

(defun powerset (set)
  (if set
      (loop for x in (powerset (cdr set))
            append (list (cons (car set) x) x))
      (list nil)))
