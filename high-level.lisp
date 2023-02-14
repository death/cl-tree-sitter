;;;; +----------------------------------------------------------------+
;;;; | cl-tree-sitter                                                 |
;;;; +----------------------------------------------------------------+

(defpackage #:cl-tree-sitter/high-level
  (:use #:cl #:cl-tree-sitter/low-level)
  (:import-from
   #:cffi
   #:null-pointer
   #:null-pointer-p
   #:foreign-string-to-lisp
   #:foreign-free
   #:define-foreign-library
   #:use-foreign-library
   #:foreign-funcall)
  (:import-from
   #:babel
   #:string-size-in-octets)
  (:export
   #:register-language
   #:list-all-languages
   #:tree-sitter-error
   #:tree-sitter-error-language
   #:tree-sitter-error-string
   #:tree-sitter-error-string-start
   #:tree-sitter-error-string-end
   #:cant-create-parser
   #:cant-set-language
   #:cant-parse-string
   #:parse-string
   #:node-type
   #:node-range
   #:node-children))

(in-package #:cl-tree-sitter/high-level)

;; Language registry

(defvar *language-registry*
  (make-hash-table))

(defmacro register-language (name lib &key fn-name)
  (check-type name symbol)
  (check-type fn-name (or null string))
  (let ((fn-name (or fn-name (substitute #\_ #\- (pathname-name lib))))
        (cffi-name (make-symbol (symbol-name name))))
    `(progn
       (define-foreign-library ,cffi-name
         (t (:default ,lib)))
       (use-foreign-library ,cffi-name)
       (setf (gethash ',name *language-registry*)
             (lambda ()
               (foreign-funcall ,fn-name ts-language)))
       ',name)))

(defun language-module (name)
  (funcall
   (or (gethash name *language-registry*)
       (error "Can't find tree-sitter language module with name ~S." name))))

(defun list-all-languages ()
  (loop for name being each hash-key of *language-registry*
        collect name))

;; Node representation

(defstruct (node (:type list))
  type
  range
  children)

(defun make-lisp-name (string)
  (intern (string-upcase (substitute #\- #\_ string))
          (load-time-value (find-package "KEYWORD"))))

;; Error conditions

(define-condition tree-sitter-error (error)
  ())

(define-condition cant-create-parser (tree-sitter-error)
  ())

(define-condition cant-set-language (tree-sitter-error)
  ((language :initarg :language :reader tree-sitter-error-language)))

(define-condition cant-parse-string (tree-sitter-error)
  ((string :initarg :string :reader tree-sitter-error-string)
   (string-start :initarg :string-start :reader tree-sitter-error-string-start)
   (string-end :initarg :string-end :reader tree-sitter-error-string-end)
   (language :initarg :language :reader tree-sitter-error-language)))

;; Parser

(defun parse-string (language string &key (start 0) end produce-cst (name-generator #'make-lisp-name))
  "Parse a STRING that represents LANGUAGE code using tree-sitter. START is
where to start parsing STRING. END is where to stop parsing STRING.
When PRODUCE-CST is set, the full concrete syntax tree will be produced as
opposed to the abstract syntax tree. See 'Named vs Anonymous Nodes':
http://tree-sitter.github.io/tree-sitter/using-parsers#named-vs-anonymous-nodes
NAME-GENERATOR is a function which converts a string from tree-sitter into a
desired name for use in lisp."
  (let ((parser (ts-parser-new)))
    (when (null-pointer-p parser)
      (error 'cant-create-parser))
    (unwind-protect (parse-string-with-language language string parser
                                                :start start
                                                :end end
                                                :produce-cst produce-cst
                                                :name-generator name-generator)
      (ts-parser-delete parser))))

(defun parse-string-with-language (language string parser
                                   &key (start 0) end produce-cst name-generator)
  (unless (ts-parser-set-language parser (language-module language))
    (error 'cant-set-language :language language))
  (let* ((string-start start)
         (string-end (or end (length string)))
         (string-length (string-size-in-octets string :start string-start :end string-end))
         (string-to-pass (if (plusp string-start)
                             (subseq string string-start string-end)
                             string))
         (tree (ts-parser-parse-string parser (null-pointer) string-to-pass string-length)))
    (when (null-pointer-p tree)
      (error 'cant-parse-string
             :string string
             :string-start start
             :string-end end
             :language language))
    (unwind-protect (convert-foreign-tree-to-list tree :produce-cst produce-cst
                                                       :name-generator name-generator)
      (ts-tree-delete tree))))

(defun convert-foreign-tree-to-list (tree &key produce-cst name-generator
                                     &aux did-visit-children parse-stack)
  (with-tree-cursor-pointer (cursor tree)
    ;; Closely follows tree-sitter-cli parse
    ;; implementation with a modification to
    ;; allow for production of the full CST.
    (loop
      (with-ts-node-pointer (node (ts-tree-cursor-current-node-pointer cursor))
        (let ((is-named (or produce-cst (ts-node-is-named-pointer node))))
          (cond (did-visit-children
                 (when (and is-named (second parse-stack))
                   (let ((item (pop parse-stack)))
                     (setf (node-children item)
                           (nreverse (node-children item)))
                     (push item (node-children (first parse-stack)))))
                 (cond ((ts-tree-cursor-goto-next-sibling cursor)
                        (setf did-visit-children nil))
                       ((ts-tree-cursor-goto-parent cursor)
                        (setf did-visit-children t))
                       (t
                        (let ((root (first parse-stack)))
                          (setf (node-children root)
                                (nreverse (node-children root)))
                          (return root)))))
                (t
                 (when is-named
                   (let ((start-point (ts-node-start-point-pointer node))
                         (end-point (ts-node-end-point-pointer node))
                         (type (funcall name-generator (ts-node-type-pointer node)))
                         (field-name-ptr (ts-tree-cursor-current-field-name cursor)))
                     (unless (null-pointer-p field-name-ptr)
                       (let ((field-name (foreign-string-to-lisp field-name-ptr)))
                         (setf type (list (funcall name-generator field-name) type))))
                     (push (make-node :type type
                                      :range (list (list (second start-point) (fourth start-point))
                                                   (list (second end-point) (fourth end-point))))
                           parse-stack)))
                 (setf did-visit-children
                       (not (ts-tree-cursor-goto-first-child cursor))))))))))
