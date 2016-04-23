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

;;;; operations.lisp

(in-package #:automata-tools)

(defgeneric product-automaton (a1 a2 &key variation))

(defmethod product-automaton ((a1 dfa) (a2 dfa) &key variation)
  (flet ((combine-pairs (pairs)
           ;; Todo: Remove combine-states in favour of the more general combined-state
           (mapcar (lambda (p) (combine-states (car p) (cdr p))) pairs)))
    (let* ((new-states (cartesian-cons (get-states a1)
                                       (get-states a2)))
           (new-alphabet (intersection (get-alphabet a1)
                                       (get-alphabet a2)))
           (new-transitions
             (loop for pair in new-states
                   collect
                   (let ((s1 (car pair)) (s2 (cdr pair)))
                     (cons (combine-states s1 s2)
                           (loop for symbol in new-alphabet
                                 collect (list symbol
                                               (combine-states (car (delta a1 s1 symbol))
                                                               (car (delta a2 s2 symbol))))))))))
      (make-automaton 'dfa
                      new-transitions
                      (combine-states (get-initial-state a1)
                                      (get-initial-state a2))
                      (if variation
                          (remove-duplicates
                           (combine-pairs
                            (append (cartesian-cons (get-accepting-states a1)
                                                    (get-states a2))
                                    (cartesian-cons (get-states a1)
                                                    (get-accepting-states a2)))))
                          (combine-pairs (cartesian-cons (get-accepting-states a1)
                                                         (get-accepting-states a2))))
                      :additional-states (combine-pairs new-states)))))

(defgeneric complement-automaton (automaton))

(defmethod complement-automaton ((a dfa))
  (make-automaton 'dfa
                  (get-transition-table a)
                  (get-initial-state a)
                  (set-difference (get-states a) (get-accepting-states a))
                  :additional-states (get-states a)))

(defgeneric to-dfa (automaton))

(defmethod to-dfa ((a nfa))
  (let ((states (powerset (get-states a)))
        (alphabet (get-alphabet a)))
    (flet ((compute-transitions (state)
             (flet ((map-symbol (symbol)
                      (and state
                           (list symbol (combined-state
                                         (reduce #'union
                                                 (mapcar (lambda (s) (delta a s symbol))
                                                         state)))))))
               (cons (combined-state state)
                     (mapcar #'map-symbol alphabet)))))
      (make-automaton 'dfa
                      (mapcar #'compute-transitions states)
                      (get-initial-state a)
                      (mapcar #'combined-state
                              (remove-if-not (lambda (s)
                                               (intersection
                                                (get-accepting-states a) s))
                                             states))))))

(defmethod to-dfa ((a nfa))
  (let* ((states (powerset (get-states a)))
         (accepting-states (reduce #'union
                                   (loop for as in (get-accepting-states a)
                                         collect (mapcar #'combined-state
                                                         (remove-if-not
                                                          (lambda (f) (find as f))
                                                          states)))))
         (alphabet (get-alphabet a))
         (dfa (make-instance 'dfa
                             :accepting-states accepting-states
                             :states (mapcar #'combined-state states)
                             :initial-state (get-initial-state a)
                             :alphabet alphabet)))
    (dolist (state states)
      (dolist (symbol alphabet)
        (eval
         `(defmethod delta ((a (eql ,dfa))
                            (state (eql (quote ,(combined-state state))))
                            (symbol (eql (quote ,symbol))))
            (quote ,(list (combined-state (reduce #'union
                                                  (loop for s in state
                                                        collect (delta a s symbol))
                                                  :initial-value nil))))))))
    dfa))

(defgeneric get-accessible-states (automaton state))

(defmethod get-accessible-states ((automaton automaton) state)
  (let ((alphabet (get-alphabet automaton)))
    (labels ((a (states tested accessible)
               (if states
                   (let ((next-states (reduce #'union
                                              (loop for symbol in alphabet
                                                    collect (delta automaton
                                                                   (car states)
                                                                   symbol)))))
                     (a (set-difference (union next-states (cdr states))
                                        tested)
                        (union tested (list (car states)))
                        (union accessible (union (list (car states)) next-states))))
                   accessible)))
      (sort (copy-list (a (list state) nil nil)) #'string-lessp))))

(defgeneric prune-automaton (automaton))
