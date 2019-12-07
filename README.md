# cl-tree-sitter

Use [tree-sitter](https://tree-sitter.github.io/) via Common Lisp.

You'll need to use [my CFFI fork](https://github.com/death/cffi), as
well as compile tree-sitter and the parser modules as shared
libraries and make sure CFFI finds them.

# Example

```lisp
CL-USER> (cl-tree-sitter:register-language :c "tree-sitter-c")
:C
CL-USER> (cl-tree-sitter:parse-string :c "int main() { return 42; }")
(:TRANSLATION-UNIT ((0 0) (25 0))
 ((:FUNCTION-DEFINITION ((0 0) (25 0))
   (((:TYPE :PRIMITIVE-TYPE) ((0 0) (3 0)) NIL)
    ((:DECLARATOR :FUNCTION-DECLARATOR) ((4 0) (10 0))
     (((:DECLARATOR :IDENTIFIER) ((4 0) (8 0)) NIL)
      ((:PARAMETERS :PARAMETER-LIST) ((8 0) (10 0)) NIL)))
    ((:BODY :COMPOUND-STATEMENT) ((11 0) (25 0))
     ((:RETURN-STATEMENT ((13 0) (23 0))
       ((:NUMBER-LITERAL ((20 0) (22 0)) NIL)))))))))
CL-USER> (cl-tree-sitter:register-language :javascript "tree-sitter-javascript")
:JAVASCRIPT
CL-USER> (cl-tree-sitter:parse-string :javascript "(function() { console.log('oops'); })()")
(:PROGRAM ((0 0) (39 0))
 ((:EXPRESSION-STATEMENT ((0 0) (39 0))
   ((:CALL-EXPRESSION ((0 0) (39 0))
     (((:FUNCTION :PARENTHESIZED-EXPRESSION) ((0 0) (37 0))
       ((:FUNCTION ((1 0) (36 0))
         (((:PARAMETERS :FORMAL-PARAMETERS) ((9 0) (11 0)) NIL)
          ((:BODY :STATEMENT-BLOCK) ((12 0) (36 0))
           ((:EXPRESSION-STATEMENT ((14 0) (34 0))
             ((:CALL-EXPRESSION ((14 0) (33 0))
               (((:FUNCTION :MEMBER-EXPRESSION) ((14 0) (25 0))
                 (((:OBJECT :IDENTIFIER) ((14 0) (21 0)) NIL)
                  ((:PROPERTY :PROPERTY-IDENTIFIER) ((22 0) (25 0)) NIL)))
                ((:ARGUMENTS :ARGUMENTS) ((25 0) (33 0))
                 ((:STRING ((26 0) (32 0)) NIL)))))))))))))
      ((:ARGUMENTS :ARGUMENTS) ((37 0) (39 0)) NIL)))))))
```

# License

MIT
