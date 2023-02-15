;;;; +----------------------------------------------------------------+
;;;; | cl-tree-sitter                                                 |
;;;; +----------------------------------------------------------------+

(in-package :asdf-user)

(asdf:defsystem #:cl-tree-sitter
  :description "Tree-Sitter bindings for Common Lisp"
  :author "death <github.com/death>"
  :license "MIT"
  :defsystem-depends-on ("asdf-package-system")
  :components ((:file "low-level")
               (:file "high-level")
               (:file "all"))
  :depends-on ("cffi-libffi"))

(defmethod perform :before ((op prepare-op)
                            (system (eql (find-system :cl-tree-sitter))))
  ;; Compile the tree-sitter wrapper functions and add it to the relevant path.
  (let ((cwd (uiop:getcwd)))
    (unwind-protect
         (progn
           (uiop:chdir (asdf:system-relative-pathname :cl-tree-sitter ""))
           (uiop:run-program "make"
                             :output *standard-output*
                             :error-output *standard-output*))
      (uiop:chdir cwd))))
