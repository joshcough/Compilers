#lang planet plai/plai:1:8
(print-only-errors #t)

;But if you're looking for a puzzle, here's one: imagine I take away
;your ability use addition and subtraction: can you implement WAE?
;(I'll let you write some little function that translates a constant
;into whatever you use to represent numbers, but that's all). That's
;not too hard if you think about numbers as, say, lists of booleans.
;But lets say that I only give you functions. Can you still do it?
;(I'll weaken the requirements a little by saying that you only have to
;get it right when the original program has no runtime errors.)

(define-type WAE
  [church-num (n church-numeral?)]
  [add (lhs WAE?)(rhs WAE?)]
  [sub (lhs WAE?)(rhs WAE?)]
  [with (name symbol?)(named-expr WAE?)(body WAE?)]
  [id (name symbol?)])

(define-type SymNumPair [symNumPair (name symbol?)(num number?)])

(define (parse sexpr)
  (cond
    [(number? sexpr) (church-num (const->church-num sexpr))]
    [(symbol? sexpr) (id sexpr)]
    [(list? sexpr)
     (case (first sexpr)
       [(+) (add (parse (second sexpr)) (parse (third sexpr)))]
       [(-) (sub (parse (second sexpr)) (parse (third sexpr)))]
       [(with) (with (first (second sexpr)) 
                     (parse (second (second sexpr))) 
                     (parse(third sexpr)) )]
       )]
    [else (error "unexpected token")]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; church numeral functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 0 -> \x\y.y
; 1 -> \x\y.xy
; 2 -> \x\y.x(xy)
; 3 -> \x\y.x(x(xy))
(define (church-numeral? n) #t)
(define (succ x) (lambda (y z) (y (x y z))))
(define (const->church-num n)
  (define (inner n acc)(if (= 0 n) acc (inner (- n 1) (succ acc))))
  (inner n zero))
(define zero (lambda (x y) y))
(define one (succ zero))
(define (church-num->const cn)(cn (lambda (x)(+ 1 x)) 0))

(define (church-add w z)(lambda (y x) (w y (z y x))))
(define (church-sub x y) (error "implement me"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interp : WAE deferred-subs -> church-numeral
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (interp expr ds)
  (type-case WAE expr
    [church-num (n) n]
    [add (l r) (church-add (interp l ds) (interp r ds))]
    [sub (l r) (church-sub (interp l ds) (interp r ds))]
    [with (bound-id named-expr body-expr)
      (interp body-expr 
              ;; add to the front of the deferred subs repo.
              (cons (symNumPair bound-id (interp named-expr ds)) ds))]
    [id (name) (lookup name ds)]
    ))

(define (top-level-interp exp) (church-num->const (interp exp '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup and utilities below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (lookup name ds) 
  (symNumPair-num (find-or-die 
    (lambda (f) (symbol=? (symNumPair-name f) name)) ds "free identifier")))

;; utility types and functions
;; Scala's Option, Maybe in Haskell.
(define-type Option [none] [some (v (lambda (x) #t))])
  
;; Like findf, but returns the (some element) or (none) 
;; instead of the element or #f.
;; i did this because i don't feel comfortable with findf returning false
;; what if the very thing that you are looking for is #f ?
;; how would you know if #f was in the list or not?
;; find [T]: (T => Boolean) List[T] -> Option[T]
(define (findo p l)
  (cond [(empty? l) (none)]
        [(p (first l)) (some (first l))]
        [else (findo p (cdr l))]))
;; find-or-die: predicate list[T] string -> T
(define (find-or-die p l error-string)
  (type-case Option (findo p l) [some (v) v] [none () (error error-string)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (top-level-interp (parse 0)) 0)
(test (top-level-interp (parse '(+ 0 0))) 0)
(test (top-level-interp (parse '(+ 1 2))) 3)
(test (top-level-interp (parse '(+ 50 50))) 100)
(test (top-level-interp (parse '(+ 50 (+ 50 50)))) 150)