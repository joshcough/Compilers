#lang slideshow

(require slideshow/code)
(require scheme/pretty)
(require unstable/gui/slideshow)

(define (large-text txt)
  (text txt (current-main-font) 62))

(define (pretty-syntax sexp . cols)
  (let ((port (open-input-string (apply pretty-format (cons sexp cols)))))
    (port-count-lines! port)
    (read-syntax "<no-name>" port)))

(define-syntax code-reduce
  (syntax-rules ()
    ((_ exp) 
     (let ((t exp))
       (hb-append (code exp) (tt " => ") (code (unsyntax t)))))))

(define comma ", ")

(slide
 (para #:align 'center 
       (large-text "L4 Compiler"))
 (blank 50)
 (vc-append 0
            (t "Josh Cough")))

(slide
 #:title "Compiler Function"
 (item (t "compile :: String -> String"))
 'next
 (subitem (t "read :: String -> Any  ;; an s-expression"))
 'next
 (subitem (t "parse :: Any -> L4"))
 'next
 (subitem (t "changeVarNames :: L4 -> L4"))
 'next
 (subitem (t "find :: E -> Context -> E"))
 'next
 (subitem (t "fill :: E -> Context -> E"))
 'next
 (subitem (t "print :: L4 -> String"))
 )

(slide
 #:title "Compiler Function"
 (item (t "compile :: String -> String"))
 (subitem (colorize (t "read :: String -> Any  ;; an s-expression") "red" ))
 (subitem (colorize (t "parse :: Any -> L4") "red" ))
 (subitem (colorize (t "changeVarNames :: L4 -> L4") "green" ))
 (subitem (colorize (t "find :: E -> Context -> E") "green" ))
 (subitem (colorize (t "fill :: E -> Context -> E") "green" ))
 (subitem (colorize (t "print :: L4 -> String") "red"))
 )

(slide
 #:title "L4 AST"
 (bitmap "./L4AST.png")
 )

(slide
 (para #:align 'center 
       (large-text "Contexts"))
 (blank 50)
 (item (t "LetContext, IfContext, ENContext, NoContext"))
)

(slide
 #:title "Find Strategy"
 'next
 (item (t "case EN(es):"))
 (subitem (t "create an ENContext with the es.tail"))
 (subitem (t "go down into es.head with that context"))
 'next
 (item (t "case Begin: turn it into a Let."))
 'next
 (item (t "case Let, If, V: do what we did in class")) 
)

(slide
 #:title "Fill Strategy"
 'next
 (item (t "case ENContext:"))
 'next
 (subitem (t "If we can go right, go right and then down."))
 'next
 (subitem (t "If there are no expressions left, go up."))
 'next
 (item (t "case LetContext, IfContext NoContext:"))
 'next
 (subitem (t "do what we did in class"))
)



(slide
 (para #:align 'center 
       (large-text "Code Time!"))
)

(slide #:title "L4 In Racket")

(slide
 #:title "L4 In Racket: Context"
 (code 
(define-type context
  [let-ctxt (x symbol?) (b L4-e?) (k context?)]
  [if-ctxt (t L4-e?) (e L4-e?) (k context?)]
  [en-ctxt 
   (es (listof L4-e?)) 
   (vs (listof val?)) 
   (k context?)]
  [no-ctxt])))

(slide
 #:title "L4 In Racket: Find"
 (code 
(define (find e k)
  (match e
    [`(let ([,x ,r]) ,b) 
     (find r (let-ctxt x b k))]
    [`(if ,c ,t ,e) 
     (find c (if-ctxt t e k))]
    [`(begin ,e1 ,e2) 
     (find `(let ([,(fresh-var) ,e1]) ,e2) k)]
    [`(,e ,es ...) 
     (find e (en-ctxt es `() k))]
    [(? val?) (fill e k)]))))

(slide
 #:title "L4 In Racket: Fill"
 (code 
(define (fill d k)
  (type-case context k
    [let-ctxt (x b k) `(let ([,x ,d]),(find b k))]
    [if-ctxt (t e k) 
     (maybe-let d (λ (v) 
       `(if ,v ,(find t k) ,(find e k))))]
    [en-ctxt (es vs k)
      (maybe-let d 
        (λ (v) 
          (define vs* (append vs (list v)))
          (match es
            ['() (fill vs* k)]
            [(cons e es*) 
             (find e (en-ctxt es* vs* k))])))]
    [no-ctxt () d]))))


#;(slide
 #:title "L4 Compiler Class Diagram"
 (bitmap "/Users/josh/Downloads/L4_1.png")
 )

#;(slide
 #:title "Forget About These"
 (bitmap "/Users/josh/Downloads/L4_2.png")
 )

#;(slide
  #:title "Hey"
 (ht-append 40
  (vl-append (rectangle 150 40) (rectangle 150 150)) (vl-append (rectangle 150 40) (rectangle 150 150)) (vl-append (rectangle 150 40) (rectangle 150 150)))
 (vl-append (rectangle 150 40) (rectangle 150 150))
 )