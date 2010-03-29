#lang planet plai/plai:1:14
(print-only-errors #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Id
  [id (name symbol?)])

(define-type TypeName
  [type-id (name Id?)]
  [function-type (dom (listof TypeName?)) (codom TypeName?)])

[define-type ClassName
  [class-name (name Id?)]]

(define-type ClassDef
  [class-def (name ClassName?) (class-args FormalParams?) (body ClassBody?)])

(define-type FunDef
  [fun-def (name symbol?) (args FormalParams?) (body Block?)])

(define-type TopLevelExpr
  [function-expr (f FunDef?)]
  [class-def-expr (c ClassDef?)])

(define-type SimpleExpr
  [new-expr (classname ClassName?) (args Arguments?)]
  [simpler-expr (s SimplerExpr?)])

(define-type SimplerExpr
  [id-expr (id Id?)]
  [dot-expr (lhs SimpleExpr?) (rhs SimpleExpr?) (args Arguments?)])

(define-type ClassBody
  [class-body (exprs (listof FunDef?))])

(define-type Block
  [block (exprs (listof SimpleExpr?))])

(define-type Param
  [param (name symbol?)(t FormalParams?)])

(define-type FormalParams
  [params (args (listof Param?))])

(define-type Arguments
  [args (as (listof SimpleExpr?))])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse : sexpr -> something
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parse-class-or-fundef sexpr)
  (cond
    [(list? sexpr)
     (case (first sexpr)
       [(class) (parse-class sexpr)]
       [(def) (parse-fun-def sexpr)]
       [else (error "something")]
       )]
    [else (error "unexpected token")]))

;(class Name () ())
(define (parse-class sexpr)
  (class 
      (class-name (id (second sexpr))) 
    (parse-args (third sexpr)) 
    (parse-class-body (fourth sexpr))))
  
(define (parse-args sexpr) (params (map parse-arg sexpr)))

(define (parse-arg sexpr)
  (param (first sexpr) (parse-type-name (second sexpr))))

; either 'someId
; or
; type-name -> type-name   (ad infinitum)
(define (parse-type-name sexpr)
  (cond
    [(symbol? sexpr) (type-id (id (first sexpr)))]
    [else (function-type (parse-type-name (first sexpr))
                         (parse-type-name (third sexpr)))]))

(define (parse-class-body sexpr) (block (map parse-fun-def sexpr)))
(define (parse-block sexpr) (block (map parse-simple-expr sexpr)))

(define (parse-fun-def sexpr)
  (fun-def (second sexpr) (parse-block third sexpr)))

(define (parse-simple-expr sexpr)
  (cond
    [(list? sexpr)
     (case (first sexpr)
       [(new) (new-expr (class-name (id (second sexpr))) 
                        (parse-args (cdr (cdr args))))]
       [else (simpler-expr (parse-simpler-expr sexpr))])]
    [else (simpler-expr (parse-simpler-expr sexpr))]))

(define (parse-simpler-expr sexpr)
  (cond
    [(symbol? sexpr) (id-expr (id sexpr))]
    [else 
     (case (first sexpr)
       [(dot) (dot-expr (parse-simple-expr (first sexpr)) 
                        (parse-simpler-expr (second sexpr))
                        (parse-args (third sexpr)))]
       [else (error "bad expr")])]))
