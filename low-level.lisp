;;;; +----------------------------------------------------------------+
;;;; | cl-tree-sitter                                                 |
;;;; +----------------------------------------------------------------+

(defpackage #:cl-tree-sitter/low-level
  (:use #:cl)
  (:import-from
   #:cffi
   #:define-foreign-library
   #:use-foreign-library
   #:defctype
   #:defcenum
   #:defcstruct
   #:defcfun
   #:with-foreign-object
   #:foreign-slot-value)
  (:export
   #:tree-sitter
   #:ts-parser
   #:ts-language
   #:ts-tree
   #:ts-symbol
   #:ts-field-id
   #:ts-input-encoding
   #:ts-symbol-type
   #:ts-log-type
   #:ts-query-predicate-step-type
   #:ts-query-error
   #:ts-point
   #:row
   #:column
   #:ts-range
   #:ts-input
   #:ts-logger
   #:ts-input-edit
   #:ts-node
   #:ts-tree-cursor
   #:ts-query-capture
   #:ts-query-match
   #:ts-query-predicate-step
   #:ts-parser-new
   #:ts-parser-delete
   #:ts-parser-set-language
   #:ts-parser-language
   #:ts-parser-set-included-ranges
   #:ts-parser-included-ranges
   #:ts-parser-parse
   #:ts-parser-parse-string
   #:ts-parser-parse-string-encoding
   #:ts-parser-reset
   #:ts-parser-set-timeout-micros
   #:ts-parser-timeout-micros
   #:ts-parser-set-cancellation-flag
   #:ts-parser-set-logger
   #:ts-parser-logger
   #:ts-parser-print-dot-graphs
   #:ts-parser-halt-on-error
   #:ts-tree-copy
   #:ts-tree-delete
   #:ts-tree-root-node
   #:ts-tree-language
   #:ts-tree-edit
   #:ts-tree-get-changed-ranges
   #:ts-node-type
   #:ts-node-symbol
   #:ts-node-start-byte
   #:ts-node-start-point
   #:ts-node-end-byte
   #:ts-node-end-point
   #:ts-node-string
   #:ts-node-parent
   #:ts-node-child
   #:ts-node-child-count
   #:ts-node-named-child
   #:ts-node-named-child-count
   #:ts-node-child-by-field-name
   #:ts-node-child-by-field-id
   #:ts-node-next-sibling
   #:ts-node-prev-sibling
   #:ts-node-next-named-sibling
   #:ts-node-prev-named-sibling
   #:ts-node-first-child-for-byte
   #:ts-node-first-named-child-for-byte
   #:ts-node-descendant-for-byte-range
   #:ts-node-descendant-for-point-range
   #:ts-node-named-descendant-for-byte-range
   #:ts-node-named-descendant-for-point-range
   #:ts-node-edit
   #:ts-tree-cursor-new
   #:ts-tree-cursor-delete
   #:with-tree-cursor
   #:ts-tree-cursor-reset
   #:ts-tree-cursor-current-node
   #:ts-tree-cursor-current-field-name
   #:ts-tree-cursor-current-field-id
   #:ts-tree-cursor-goto-parent
   #:ts-tree-cursor-goto-next-sibling
   #:ts-tree-cursor-goto-first-child
   #:ts-tree-cursor-goto-first-child-for-byte
   #:ts-tree-cursor-copy
   #:ts-language-symbol-count
   #:ts-language-symbol-name
   #:ts-language-symbol-for-name
   #:ts-language-field-count
   #:ts-language-field-name-for-id
   #:ts-language-field-id-for-name
   #:ts-language-symbol-type
   #:ts-language-version
   #:with-ts-node-pointer
   #:with-tree-cursor-pointer
   #:ts-tree-cursor-new-pointer
   #:ts-tree-root-node-pointer
   #:ts-node-is-named-pointer
   #:ts-tree-cursor-current-node-pointer
   #:ts-node-start-point-pointer
   #:ts-node-end-point-pointer
   #:ts-node-type-pointer))

(in-package #:cl-tree-sitter/low-level)

;; Library

(define-foreign-library tree-sitter
  (:darwin (:default "/usr/local/lib/libtree-sitter"))
  (t (:or (:default "tree-sitter") (:default "libtree-sitter"))))

(use-foreign-library tree-sitter)

(define-foreign-library (tree-sitter-wrapper
                         :search-path
                         (asdf:system-relative-pathname :cl-tree-sitter ""))
  (t (:default "tree-sitter-wrapper")))

(use-foreign-library tree-sitter-wrapper)

;; Types

(defctype ts-parser :pointer)

(defctype ts-language :pointer)

(defctype ts-tree :pointer)

(defctype ts-symbol :uint16)

(defctype ts-field-id :uint16)

;; Enums

(defcenum ts-input-encoding
  :utf-8
  :utf-16)

(defcenum ts-symbol-type
  :regular
  :anonymous
  :auxiliary)

(defcenum ts-log-type
  :parse
  :lex)

(defcenum ts-query-predicate-step-type
  :done
  :capture
  :string)

(defcenum ts-query-error
  :none
  :syntax
  :node-type
  :field
  :capture)

;; Structs

(defcstruct ts-point
  (row :uint32)
  (column :uint32))

(defcstruct ts-range
  (start-point (:struct ts-point))
  (end-point (:struct ts-point))
  (start-byte :uint32)
  (end-byte :uint32))

(defcstruct ts-input
  (payload :pointer)
  (read :pointer)
  (encoding ts-input-encoding))

(defcstruct ts-logger
  (payload :pointer)
  (log :pointer))

(defcstruct ts-input-edit
  (start-byte :uint32)
  (old-end-byte :uint32)
  (new-end-byte :uint32)
  (start-point (:struct ts-point))
  (old-end-point (:struct ts-point))
  (new-end-point (:struct ts-point)))

(defcstruct ts-node
  (context :uint32 :count 4)
  (id :pointer)
  (tree ts-tree))

(defcstruct ts-tree-cursor
  (tree :pointer)
  (id :pointer)
  (context :uint32 :count 2))

(defcstruct ts-query-capture
  (node (:struct ts-node))
  (index :uint32))

(defcstruct ts-query-match
  (id :uint32)
  (pattern-index :uint16)
  (capture-count :uint16)
  (captures (:pointer (:struct ts-query-capture))))

(defcstruct ts-query-predicate-step
  (type ts-query-predicate-step-type)
  (value-id :uint32))

;; Functions

(defcfun ts-parser-new ts-parser)

(defcfun ts-parser-delete :void
  (parser ts-parser))

(defcfun ts-parser-set-language :boolean
  (parser ts-parser)
  (language ts-language))

(defcfun ts-parser-language ts-language
  (parser ts-parser))

(defcfun ts-parser-set-included-ranges :void
  (parser ts-parser)
  (ranges (:pointer (:struct ts-range)))
  (length :uint32))

(defcfun ts-parser-included-ranges (:pointer (:struct ts-range))
  (parser ts-parser)
  (length :uint32))

(defcfun ts-parser-parse ts-tree
  (parser ts-parser)
  (old-tree ts-tree)
  (input (:struct ts-input)))

(defcfun ts-parser-parse-string ts-tree
  (parser ts-parser)
  (old-tree ts-tree)
  (string :string)
  (length :uint32))

(defcfun ts-parser-parse-string-encoding ts-tree
  (parser ts-parser)
  (old-tree ts-tree)
  (string :string)
  (length :uint32)
  (encoding ts-input-encoding))

(defcfun ts-parser-reset :void
  (parser ts-parser))

(defcfun ts-parser-set-timeout-micros :void
  (parser ts-parser)
  (timeout :uint64))

(defcfun ts-parser-timeout-micros :uint64
  (parser ts-parser))

(defcfun ts-parser-set-cancellation-flag :pointer
  (parser ts-parser))

(defcfun ts-parser-set-logger :void
  (parser ts-parser)
  (logger (:struct ts-logger)))

(defcfun ts-parser-logger (:struct ts-logger)
  (parser ts-parser))

(defcfun ts-parser-print-dot-graphs :void
  (parser ts-parser)
  (file :int))

(defcfun ts-parser-halt-on-error :void
  (parser ts-parser)
  (halt :boolean))

(defcfun ts-tree-copy ts-tree
  (tree ts-tree))

(defcfun ts-tree-delete :void
  (tree ts-tree))

(defcfun ts-tree-root-node (:struct ts-node)
  (tree ts-tree))

(defcfun ts-tree-language ts-language
  (tree ts-tree))

(defcfun ts-tree-edit :void
  (tree ts-tree)
  (edit (:struct ts-input-edit)))

(defcfun ts-tree-get-changed-ranges (:pointer (:struct ts-range))
  (old-tree ts-tree)
  (new-tree ts-tree)
  (length :pointer))

(defcfun ts-node-type :string
  (node (:struct ts-node)))

(defcfun ts-node-symbol ts-symbol
  (node (:struct ts-node)))

(defcfun ts-node-start-byte :uint32
  (node (:struct ts-node)))

(defcfun ts-node-start-point (:struct ts-point)
  (node (:struct ts-node)))

(defcfun ts-node-end-byte :uint32
  (node (:struct ts-node)))

(defcfun ts-node-end-point (:struct ts-point)
  (node (:struct ts-node)))

(defcfun ts-node-string :pointer
  (node (:struct ts-node)))

(defcfun ts-node-parent (:struct ts-node)
  (node (:struct ts-node)))

(defcfun ts-node-child (:struct ts-node)
  (node (:struct ts-node))
  (index :uint32))

(defcfun ts-node-child-count :uint32
  (node (:struct ts-node)))

(defcfun ts-node-named-child (:struct ts-node)
  (node (:struct ts-node))
  (index :uint32))

(defcfun ts-node-named-child-count :uint32
  (node (:struct ts-node)))

(defcfun ts-node-child-by-field-name (:struct ts-node)
  (self (:struct ts-node))
  (field-name :string)
  (field-name-length :uint32))

(defcfun ts-node-child-by-field-id (:struct ts-node)
  (node (:struct ts-node))
  (id ts-field-id))

(defcfun ts-node-next-sibling (:struct ts-node)
  (node (:struct ts-node)))

(defcfun ts-node-prev-sibling (:struct ts-node)
  (node (:struct ts-node)))

(defcfun ts-node-next-named-sibling (:struct ts-node)
  (node (:struct ts-node)))

(defcfun ts-node-prev-named-sibling (:struct ts-node)
  (node (:struct ts-node)))

(defcfun ts-node-first-child-for-byte (:struct ts-node)
  (node (:struct ts-node))
  (index :uint32))

(defcfun ts-node-first-named-child-for-byte (:struct ts-node)
  (node (:struct ts-node))
  (index :uint32))

(defcfun ts-node-descendant-for-byte-range (:struct ts-node)
  (node (:struct ts-node))
  (start :uint32)
  (end :uint32))

(defcfun ts-node-descendant-for-point-range (:struct ts-node)
  (node (:struct ts-node))
  (start (:struct ts-point))
  (end (:struct ts-point)))

(defcfun ts-node-named-descendant-for-byte-range (:struct ts-node)
  (node (:struct ts-node))
  (start :uint32)
  (end :uint32))

(defcfun ts-node-named-descendant-for-point-range (:struct ts-node)
  (node (:struct ts-node))
  (start (:struct ts-point))
  (end (:struct ts-point)))

(defcfun ts-node-edit :void
  (node (:pointer (:struct ts-node)))
  (edit (:pointer (:struct ts-input-edit))))

(defcfun ts-tree-cursor-new (:struct ts-tree-cursor)
  (node (:struct ts-node)))

(defcfun ts-tree-cursor-delete :void
  (cursor (:pointer (:struct ts-tree-cursor))))

(defmacro with-tree-cursor ((var node) &body forms)
  (let ((cursor-lisp (gensym)))
    `(let ((,cursor-lisp (ts-tree-cursor-new ,node)))
       (with-foreign-object (,var '(:struct ts-tree-cursor))
         (setf (foreign-slot-value ,var '(:struct ts-tree-cursor) 'tree)
               (getf ,cursor-lisp 'tree))
         (setf (foreign-slot-value ,var '(:struct ts-tree-cursor) 'id)
               (getf ,cursor-lisp 'id))
         (setf (foreign-slot-value ,var '(:struct ts-tree-cursor) 'context)
               (getf ,cursor-lisp 'context))
         (unwind-protect
              (progn ,@forms)
           (ts-tree-cursor-delete ,var))))))

(defcfun ts-tree-cursor-reset :void
  (cursor (:pointer (:struct ts-tree-cursor)))
  (node (:struct ts-node)))

(defcfun ts-tree-cursor-current-node (:struct ts-node)
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun ts-tree-cursor-current-field-name :pointer
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun ts-tree-cursor-current-field-id ts-field-id
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun ts-tree-cursor-goto-parent :boolean
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun ts-tree-cursor-goto-next-sibling :boolean
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun ts-tree-cursor-goto-first-child :boolean
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun ts-tree-cursor-goto-first-child-for-byte :int64
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun ts-tree-cursor-copy (:struct ts-tree-cursor)
  (cursor (:pointer (:struct ts-tree-cursor))))

;; TODO: Query section

(defcfun ts-language-symbol-count :uint32
  (language ts-language))

(defcfun ts-language-symbol-name :string
  (language ts-language)
  (symbol ts-symbol))

(defcfun ts-language-symbol-for-name ts-symbol
  (language ts-language)
  (string :string)
  (length :uint32)
  (is-named :boolean))

(defcfun ts-language-field-count :uint32
  (language ts-language))

(defcfun ts-language-field-name-for-id :string
  (language ts-language)
  (field-id ts-field-id))

(defcfun ts-language-field-id-for-name ts-field-id
  (language ts-language)
  (name :string)
  (length :uint32))

(defcfun ts-language-symbol-type ts-symbol-type
  (language ts-language)
  (symbol ts-symbol))

(defcfun ts-language-version :uint32
  (language ts-language))

;; tree-sitter wrapper

(defmacro with-ts-node-pointer ((var node) &body forms)
  `(let ((,var ,node))
     (unwind-protect
          (progn ,@forms)
       (cffi-sys:foreign-free ,var))))

(defmacro with-tree-cursor-pointer ((var tree) &body forms &aux (node (gensym)))
  `(with-ts-node-pointer (,node (ts-tree-root-node-pointer ,tree))
     (let ((,var (ts-tree-cursor-new-pointer ,node)))
       (unwind-protect
            (progn ,@forms)
         (ts-tree-cursor-delete ,var)))))

(defcfun ts-tree-root-node-pointer (:pointer (:struct ts-node))
  (tree ts-tree))

(defcfun ts-tree-cursor-new-pointer (:pointer (:struct ts-tree-cursor))
  (node (:pointer (:struct ts-node))))

(defcfun ts-node-is-named-pointer :boolean
  (node (:pointer (:struct ts-node))))

(defcfun ts-tree-cursor-current-node-pointer (:pointer (:struct ts-node))
  (cursor (:pointer (:struct ts-tree-cursor))))

(defcfun ts-node-start-point-pointer (:struct ts-point)
  (node (:pointer (:struct ts-node))))

(defcfun ts-node-end-point-pointer (:struct ts-point)
  (node (:pointer (:struct ts-node))))

(defcfun ts-node-type-pointer :string
  (node (:pointer (:struct ts-node))))
