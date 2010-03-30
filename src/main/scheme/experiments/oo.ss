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
  [fun-def (name Id?) (args FormalParams?) (body Block?)])

(define-type TopLevelExpr
  [function-expr (f FunDef?)]
  [class-def-expr (c ClassDef?)])

(define-type SimpleExpr
  [new-expr (classname ClassName?) (args Arguments?)]
  [simpler-expr (s SimplerExpr?)])

(define-type SimplerExpr
  [id-expr (id Id?)]
  [string-lit-expr (s string?)]
  [dot-expr (lhs SimpleExpr?) (rhs SimplerExpr?) (args Arguments?)])

(define-type ClassBody
  [class-body (exprs (listof FunDef?))])

(define-type Block
  [block (exprs (listof SimpleExpr?))])

(define-type Param
  [param (name Id?)(t TypeName?)])

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
  (class-def
      (class-name (id (second sexpr))) 
    (parse-formal-params (third sexpr)) 
    (parse-class-body (fourth sexpr))))

(define (parse-formal-params sexpr) 
  (params (map parse-formal-param sexpr)))

(define (parse-formal-param sexpr)
  (param (id (first sexpr)) (parse-type-name (third sexpr))))

(define (parse-args sexpr) (args (map parse-simple-expr sexpr)))

; either 'someId or type-name -> type-name   (ad infinitum)
(define (parse-type-name sexpr)
  (cond
    [(symbol? sexpr) (type-id (id sexpr))]
    [else (function-type (parse-type-name (first sexpr))
                         (parse-type-name (third sexpr)))]))

(define (parse-class-body sexpr) (class-body (map parse-fun-def sexpr)))
(define (parse-block sexpr) (block (map parse-simple-expr sexpr)))

(define (parse-fun-def sexpr)
  (fun-def (id (second sexpr)) 
           (parse-formal-params (third sexpr)) 
           (parse-block (fourth sexpr))))

(define (parse-simple-expr sexpr)
  (cond
    [(list? sexpr)
     (case (first sexpr)
       [(new) (new-expr (class-name (id (second sexpr))) 
                        (parse-args (third sexpr)))]
       [else (simpler-expr (parse-simpler-expr sexpr))])]
    [else (simpler-expr (parse-simpler-expr sexpr))]))

(define (parse-simpler-expr sexpr)
  (cond
    [(symbol? sexpr) (id-expr (id sexpr))]
    [(string? sexpr) (string-lit-expr sexpr)]
    [else 
     (case (second sexpr)
       [(dot) (dot-expr (parse-simple-expr (first sexpr)) 
                        (parse-simpler-expr (third sexpr))
                        (parse-args (fourth sexpr)))]
       [else (error "bad expr")])]))


(test (parse-simpler-expr 'x) (id-expr (id 'x)))
(test (parse-simpler-expr "hey") (string-lit-expr "hey"))
(test (parse-simpler-expr '(x dot x ())) 
      (dot-expr (simpler-expr (id-expr (id 'x))) (id-expr (id 'x)) (args '())))
(test (parse-simpler-expr '(x dot x (z))) 
      (dot-expr (simpler-expr (id-expr (id 'x))) (id-expr (id 'x)) 
                (args (list (simpler-expr (id-expr (id 'z)))))))
(test (parse-simpler-expr '(x dot x ("yes"))) 
      (dot-expr (simpler-expr (id-expr (id 'x))) (id-expr (id 'x)) 
                (args (list (simpler-expr (string-lit-expr "yes"))))))
(test (parse-simple-expr '(new String ())) (new-expr (class-name (id 'String)) (args '())))
(test (parse-simple-expr '((new String ()) dot trim ())) 
      (simpler-expr (dot-expr 
                     (new-expr (class-name (id 'String)) (args '())) 
                     (id-expr (id 'trim)) (args '())))) 

; classes 
(test (parse-class '(class Dumb () ())) 
      (class-def (class-name (id 'Dumb)) (params '()) (class-body '())))

(test (parse-class '(class Num ((x : Int)) ()))
      (class-def (class-name (id 'Num)) 
                 (params (list (param (id 'x) (type-id (id 'Int))))) 
                 (class-body '())))

(test (parse-class '(class Point ((x : Int) (y : Int)) ()))
      (class-def (class-name (id 'Point)) 
                 (params (list (param (id 'x) (type-id (id 'Int)))
                               (param (id 'y) (type-id (id 'Int))))) 
                 (class-body '())))

; fundef 
(test (parse-fun-def '(def left ((el : Int) (ar : Int)) (el))) 
      (fun-def (id 'left) 
               (params (list (param (id 'el) (type-id (id 'Int))) 
                             (param (id 'ar) (type-id (id 'Int))))) 
               (block (list (simpler-expr (id-expr (id 'el)))))))

; fundef in classdef
(test (parse-class '(class Point ((x : Int) (y : Int)) 
                      ((def left ((el : Int) (ar : Int)) (el)))))
      (class-def (class-name (id 'Point)) 
                 (params (list (param (id 'x) (type-id (id 'Int)))
                               (param (id 'y) (type-id (id 'Int))))) 
                 (class-body (list (fun-def (id 'left) 
                                            (params (list (param (id 'el) (type-id (id 'Int))) 
                                                          (param (id 'ar) (type-id (id 'Int))))) 
                                            (block (list (simpler-expr (id-expr (id 'el))))))))))
