;;;; +----------------------------------------------------------------+
;;;; | cl-tree-sitter                                                 |
;;;; +----------------------------------------------------------------+

(in-package :asdf-user)

(asdf:defsystem #:cl-tree-sitter.config
  :description "Configure tree-sitter libraries."
  :author "death <github.com/death>"
  :license "MIT"
  :defsystem-depends-on ("asdf-package-system")
  :components ((:file "config"))
  :depends-on ("cffi-libffi"))
