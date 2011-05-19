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
            ;(colorize (bt "University of Utah") "red")
            ;(colorize (t "matt.might.net") "blue")))

(slide
  (para #:align 'center 
       (large-text "TEST TEST TEST"))
 )

(slide
 #:title "L4 Test Function"
 (code 
  (:test (actual expected)
    (if (= actual expected)
      (print 1)
      (begin (print actual) (print expected))
    )
  )
 )
)

(slide
 #:title "L4 Test Function Basics"
 (colorize (item (code (:test 5 5))) "green")
 'next
 (colorize (item (code (:test (< 3 4) 1))) "green")
 'next
 (colorize (item (code (:test 5 6))) "red")
 'next
 (colorize (item (code (:test (< 3 4) 0))) "red")
 )

(slide
 #:title "Creating L4 Lists"
 (item (code (:nil)))
 'next
 (item (code (:cons 1 (:nil))))
 'next
 (item (code (:cons 1 (:cons 2 (:cons 3 (:nil))))))
 )

(slide
 #:title "Creating L4 Lists: :nil and :cons"
 'alts
 (list 
  (list (item (code (:nil () ...))))
  (list (item (code (:nil () (new-tuple))))))
 'next
 'alts
 (list 
  (list (item (code (:cons (x list) ...))))
  (list (item (code (:cons (x list) (new-tuple x list))))))
 )

(slide
 #:title "L4 List Equality Examples"
 (item (code (:test (:nil) (new-tuple))))
 'next
 (item (code (:test (:nil) (:nil))))
 'next
 (item (code 
        (:test
         (:cons 1 (:nil))
         (new-tuple 1 (new-tuple))
         )))
 'next
 (item (code 
        (:test
         (:cons 1 (:cons 2 (:cons 3 (:nil))))
         (:cons 1 (:cons 2 (:cons 3 (:nil))))
         )))
 )

(slide
 #:title "L4 Test Function Revisited"
 'alts
 (list
  (list (code 
  (:test (actual expected)
    (if (= actual expected)
      (print 1)
      (begin (print actual) (print expected))
    )
  )
 ))
  (list (code 
  (:test (actual expected)
    (if (:eq actual expected)
      (print 1)
      (begin (print actual) (print expected))
    )
  )
 ))
 )
)

(slide
 #:title "L4 Equality Basics"
 'alts
 (list 
  (list (item (code (:test (:eq 4 4) ?))))
  (list (item (code (:test (:eq 4 4) 1)))))
 'next
 'alts
 (list 
  (list (item (code (:test (:eq 4 7) ?))))
  (list (item (code (:test (:eq 4 7) 0)))))
 'next
 'alts
 (list 
  (list (item (code (:test (:eq (:nil) (:nil)) ?))))
  (list (item (code (:test (:eq (:nil) (:nil)) 1)))))
 'next
 'alts
 (list 
  (list (item (code 
    (:test (:eq 
            (:cons 7 (:nil)) 
            (:cons 7 (:nil))) 
           ?))))
  (list (item (code 
        (:test (:eq 
                (:cons 7 (:nil)) 
                (:cons 7 (:nil))) 
               1)))))
 'next
 'alts
 (list 
  (list (item (code 
        (:test (:eq 
                (:cons 7 (:nil)) 
                (:nil)) 
               ?))))
  (list (item (code 
        (:test (:eq 
                (:cons 7 (:nil)) 
                (:nil)) 
               0)))))
 
 'next
 'alts
 (list 
  (list (item (code (:test (:eq 4 (:nil)) ?))))
  (list (item (code (:test (:eq 4 (:nil)) 0)))))
 )

(slide
 #:title "Defining :eq - A few useful functions"
 (item (code (:and (x y) (if x (if y 1 0) 0))))
 (item (code (:or (x y) (if x 1 (if y 1 0)))))
 (item (code (:head (list) (aref list 0))))
 (item (code (:tail (list) (aref list 1))))
 (item (code (:empty (list) (= 0 (alen list)))))
 )

(slide
 #:title "Defining :eq"
 (item 
  (code 
   (:eq (x y)
     (if (:and (a? x) (a? y))
       (:eqlist x y)
       (if (:and (number? x) (number? y))
         (= x y) 
         0)))))
 'next
 (item 
  (code 
  (:eqlist (x y)
    (if (:and (:empty x) (:empty y))
      1
      (if (:or (:empty x) (:empty y))
        0
        (:and 
          (:eq (:head x) (:head y)) 
          (:eqlist (:tail x) (:tail y)))
      )
    )
  )))
 )


(slide
 #:title "Compiler Flow"
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
 #:title "Compiler Flow"
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
 (item (t "case class L4(main: E, funcs:List[Func])"))
 'next
 (item (t "case class Func(label:Label, args:List[Variable], body:E)"))
 'next
 (item (t "trait E"))
 'next
 (subitem (t "case class Let(v:Variable, e:E, body:E) extends E"))
 'next
 (subitem (t "Lots more Es..."))
 )

(slide
 #:title "More L4 AST"
 (item (t "trait E"))
 (subitem
  (vl-append
   (t "trait EN extends E {")
   (t "  def es: List[E]")
   (t "  def rebuild(List[E]): E")
   (t "}")
   )
  )
 (subitem (t "trait E1 extends EN { ... }"))
 (subitem (t "trait E2 extends EN { ... }"))
 )
 
(slide
 #:title "More L4 AST"
 (item (t "trait E1 extends EN"))
 (subitem (t "case class IsNumber(e:E) extends E1 { ... }"))
 (subitem (t "case class IsArray(e:E) extends E1 { ... }"))
 (subitem (t "case class Print(e:E) extends E1 { ... }"))
 'next
 (subitem (t "lots more E1s..."))
 )
 
(slide
 #:title "More L4 AST"
 (item (t "trait E2 extends EN"))
 (subitem (t "case class Add(e1:E, e2:E) extends E2 { ... }"))
 (subitem (t "case class Sub(e1:E, e2:E) extends E2 { ... }"))
 'next
 (subitem (t "lots more E2s..."))
 )


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