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

;;;; automata-tools.asd

(asdf:defsystem #:automata-tools
  :description "Learning about finite automata in Common Lisp."
  :author "Grim Schjetne"
  :license "GPLv3+"
  :serial t
  :depends-on (:cl-dot)
  :components ((:file "package")
               (:file "utils")
               (:file "automata-tools")
               (:file "deterministic")
               (:file "nondeterministic")
               (:file "operations")
               (:file "output")))

