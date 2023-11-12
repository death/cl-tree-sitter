;;;; +----------------------------------------------------------------+
;;;; | cl-tree-sitter                                                 |
;;;; +----------------------------------------------------------------+

(defpackage #:cl-tree-sitter.config
  (:use #:cl)
  (:import-from #:cffi
                #:use-foreign-library
                #:define-foreign-library)
  (:export
   #:define-tree-sitter-path
   #:define-tree-sitter-wrapper-path))

(in-package #:cl-tree-sitter.config)

(defmacro define-tree-sitter-path (path)
  `(progn
     (define-foreign-library tree-sitter
       (t ,path))
     (use-foreign-library tree-sitter)))

(defmacro define-tree-sitter-wrapper-path (path)
  `(progn
     (define-foreign-library tree-sitter-wrapper
       (t (:default ,path)))
     (use-foreign-library tree-sitter-wrapper)))


