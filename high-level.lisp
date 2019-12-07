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

(defmacro register-language (name lib)
  (check-type name symbol)
  (let ((fn-name (substitute #\_ #\- (pathname-name lib)))
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

(defun parse-string (language string &key (start 0) end)
  (let ((parser (ts-parser-new)))
    (when (null-pointer-p parser)
      (error 'cant-create-parser))
    (unwind-protect
         (progn
           (unless (ts-parser-set-language parser (language-module language))
             (error 'cant-set-language :language language))
           (let* ((string-start start)
                  (string-end (or end (length string)))
                  (string-length (- string-end string-start))
                  (string-to-pass (if (plusp string-start)
                                      (subseq string string-start string-end)
                                      string))
                  (tree (ts-parser-parse-string parser (null-pointer) string-to-pass string-length)))
             (when (null-pointer-p tree)
               (error 'cant-parser-string
                      :string string
                      :string-start start
                      :string-end end
                      :language language))
             (unwind-protect
                  (let ((root-node (ts-tree-root-node tree)))
                    (with-tree-cursor (cursor root-node)
                      ;; Closely follows tree-sitter-cli parse
                      ;; implementation.
                      (let ((did-visit-children nil)
                            (parse-stack '()))
                        (loop
                         (let* ((node (ts-tree-cursor-current-node cursor))
                                (is-named (ts-node-is-named node)))
                           (cond (did-visit-children
                                  (when is-named
                                    (when (second parse-stack)
                                      (let ((item (pop parse-stack)))
                                        (setf (node-children (first parse-stack))
                                              (append (node-children (first parse-stack))
                                                      (list item))))))
                                  (cond ((ts-tree-cursor-goto-next-sibling cursor)
                                         (setf did-visit-children nil))
                                        ((ts-tree-cursor-goto-parent cursor)
                                         (setf did-visit-children t))
                                        (t
                                         (return))))
                                 (t
                                  (when is-named
                                    (let ((start-point (ts-node-start-point node))
                                          (end-point (ts-node-end-point node))
                                          (type (make-lisp-name (ts-node-type node)))
                                          (field-name-ptr (ts-tree-cursor-current-field-name cursor)))
                                      (unless (null-pointer-p field-name-ptr)
                                        (let ((field-name (foreign-string-to-lisp field-name-ptr)))
                                          (setf type (list (make-lisp-name field-name) type))))
                                      (push (make-node :type type
                                                       :range (list (list (second start-point) (fourth start-point))
                                                                    (list (second end-point) (fourth end-point))))
                                            parse-stack)))
                                  (setf did-visit-children
                                        (not (ts-tree-cursor-goto-first-child cursor)))))))
                        (first parse-stack))))
               (ts-tree-delete tree))))
      (ts-parser-delete parser))))
