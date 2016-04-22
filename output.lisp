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

;;;; output.lisp

(in-package #:automata-tools)

(defgeneric render-diagram (automaton outfile &key format prune))

(defmethod render-diagram ((a automaton) outfile &key format prune)
  (dot-graph (generate-graph-from-roots a (cons :start (unless prune
                                                         (get-states a))))
             outfile
             :format format
             :directed t))

(defmethod graph-object-node ((a automaton) (state (eql :start)))
  (make-instance 'node :attributes '(:style :invis)))

(defmethod graph-object-points-to ((a automaton) (state (eql :start)))
  (list (get-initial-state a)))

(defgeneric latex-transition-table (automaton outfile))

(defmethod latex-transition-table ((a automaton) outfile)
  (with-open-file (f outfile :direction :output :if-exists :supersede)
    (let ((alphabet (sort (get-alphabet a) #'multi-lessp))
          (states (sort (get-states a) #'multi-lessp))
          (initial-state (get-initial-state a))
          (accepting-states (get-accepting-states a)))
      (format f "\\begin{tabular}{ r |~{| c ~*~}}~%" alphabet)
      (format f "  $\\delta$ ~{& ~A ~}\\\\~%  \\hline~%" alphabet)
      (if (deterministic-p a)
          (dolist (state states)
            (format f "  $~:[~;\\rightarrow~] ~:[~;*~] ~A$ ~{& $~:[-~;~:*~A~]$ ~}\\\\~%"
                    (eql state initial-state)
                    (find state accepting-states)
                    state
                    (mapcar (lambda (symbol) (car (delta a state symbol)))
                            alphabet)))
          (error "Not yet implemented"))
      (format f "\\end{tabular}~%"))))
