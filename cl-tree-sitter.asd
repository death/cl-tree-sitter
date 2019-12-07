;;;; +----------------------------------------------------------------+
;;;; | cl-tree-sitter                                                 |
;;;; +----------------------------------------------------------------+

(asdf:defsystem #:cl-tree-sitter
  :description "Tree-Sitter bindings for Common Lisp"
  :author "death <github.com/death>"
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("cffi-libffi"
               "cl-tree-sitter/all"))
