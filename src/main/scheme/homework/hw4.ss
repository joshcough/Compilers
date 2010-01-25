#lang planet plai/plai:1:8
(print-only-errors #t)

;  <FunDef> = {deffun {<id> <id>*} FnWAE}
;  <FnWAE> = <number>
;          | {+ FnWAE FnWAE}
;          | {- FnWAE FnWAE}
;          | {with {<id> FnWAE} FnWAE}
;          | <id>
;          | {if0 FnWAE FnWAE FnWAE}
;          | {<id> FnWAE*}
(define-type FnWAE
  [num (n number?)]
  [add (lhs FnWAE?)(rhs FnWAE?)]
  [sub (lhs FnWAE?)(rhs FnWAE?)]
  [with (name symbol?)(named-expr FnWAE?)(body FnWAE?)]
  [id (name symbol?)]
  [if0 (x FnWAE?)(y FnWAE?)(z FnWAE?)]
  [app (fun-name symbol?)(args (list-of? FnWAE?))])

(define-type FunDef
  [fundef (name symbol?)
          (arg-names list?)
          (body FnWAE?)])

(define-type SymNumPair [symNumPair (name symbol?)(num number?)])

;; utility
(define (list-of? t) (lambda (l) (andmap t l)))

;; parse : sexpr -> FnWAE
(define (parse sexpr)
  (cond
    [(number? sexpr) (num sexpr)]
    [(symbol? sexpr) (id sexpr)]
    [(list? sexpr)
     (case (first sexpr)
       [(+) (add (parse (second sexpr)) (parse (third sexpr)))]
       [(-) (sub (parse (second sexpr)) (parse (third sexpr)))]
       [(with) (parse-with sexpr)]
       [(deffun) (parse-defn sexpr)]
       [(if0) (if0 (parse (second sexpr)) 
                   (parse (third sexpr)) 
                   (parse (fourth sexpr)))]
       ;; assume any other symbols are function names, therefore application.
       [else (app (first sexpr) (map parse (cdr sexpr)))]
       )]
    [else (error "unexpected token")]))

;; parse-with : sexpr -> FnWAE
(define (parse-with sexpr)
  (with (first (second sexpr)) 
        (parse (second (second sexpr))) 
        (parse (third sexpr))))

;; parse-defn : sexpr -> FunDef
(define (parse-defn sexpr)
  (cond
    [(list? sexpr)
     (case (first sexpr)
       [(deffun) (fundef (first (second sexpr)) 
                         (check-for-dups (cdr (second sexpr)) "bad syntax")
                         (parse (third sexpr)))]
       )]
    [else (error "unexpected token")]))

(define (check-for-dups l error-message)
  (if (same-size l (remove-duplicates l symbol=?))
      l
      (error error-message)))

;; fundef-lookup : sym list-of-FunDef -> FunDef
(define (fundef-lookup fname l)
  (find-or-die
   (lambda (f) (symbol=? (fundef-name f) fname)) l 
   (string-append "undefined function: " (symbol->string fname))))

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
;; same-size: list list -> boolean
(define (same-size l r) (= (length l)(length r)))


;;;;
;; lookup
;;;;
(define (lookup name ds) 
  (symNumPair-num (find-or-die 
    (lambda (f) (symbol=? (symNumPair-name f) name)) ds "free identifier")))

(define (zip-with const ll lr)
  (cond [(empty? ll) '()]
        [(empty? lr) '()]
        [else (cons (const (first ll) (first lr)) 
                    (zip-with const (cdr ll) (cdr lr)))]))

(define (zip ll lr) (zip-with list ll lr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interp : FnWAE list-of-FunDef -> FnWAE-Val
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (interp expr defs ds)
  (type-case FnWAE expr
    [num (n) n]
    [add (l r) (+ (interp l defs ds) (interp r defs ds))]
    [sub (l r) (- (interp l defs ds) (interp r defs ds))]
    [with (bound-id named-expr body-expr)
      (interp body-expr defs 
              ;; add to the front of the deferred subs repo.
              (cons (symNumPair bound-id (interp named-expr defs ds)) ds))]
    [id (name) (lookup name ds)]
    [if0 (x y z) (if (= 0 (interp x defs ds))
                     (interp y defs ds)
                     (interp z defs ds))]
    [app (fname arg-exprs)
         (local [(define f (fundef-lookup fname defs))]
           (if (= (length (fundef-arg-names f))(length arg-exprs))
               (case (length arg-exprs)
                 [(0) (interp (fundef-body f) defs '())]
                 [else (interp 
                        (fundef-body f)
                        defs
                        ;; i wanted to make this an inner function, giving it a
                        ;; name, and therefore some abstraction, but i couldn't
                        ;; put it here, right above the where id call it.
                        ;; define: not allowed in an expression context
                        ;; i could have defined it at the top of the function
                        ;; but not only do i think thats too far away, but
                        ;; having that inner def be the first thng you see when
                        ;; looking at interp was confusing.
                        ;; i also could have just put it below this function,
                        ;; but it would have had to take defs and ds as args
                        ;; as well...alas i just left this code here. 
                        ;; it simply creates a list of symNumPairs
                        ;; syms - the names of the function's formal args
                        ;; nums - the interpreted arguments to the function
                        (zip-with symNumPair (fundef-arg-names f) 
                             (map (lambda (x) (interp x defs ds)) arg-exprs)))]
                 )
               (error "wrong arity")))]))

(define (interp-expr expr defs) (interp expr defs '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the library!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; primitive as it may be, building a library for a language you've
; built, in the language that you've built, is very exciting.
(define mult-and-neg-deffuns
  (list 
   '(deffun (or  x y) (if0 x 0 (if0 y 0 1)))
   '(deffun (and x y) (if0 x (if0 y 0 1) 1))
   '(deffun (zero? x) (if0 x 0 1))
   '(deffun (not   x) (if0 x 1 0))
   '(deffun (neg?  x) (if0 x 1 (split-neg? x x)))
   '(deffun (pos?  x) (and (not (zero? x)) (not (neg? x)))) 
   '(deffun (split-neg? p n) 
      (if0 (+ n 1) 0
           (if0 (or (or (zero? p) (zero? n)) (zero? (- p 1))) 1 
           (split-neg? (- p 1) (+ n 1)))))
   '(deffun (abs x) (if0 (neg? x) (- 0 x) x))
   '(deffun (add-n-times n x) (if0 n 0 (+ x (add-n-times (- n 1) x))))
   '(deffun (mult x y) 
      (with (n (add-n-times (abs x) y)) (if0 (pos? x) n (- 0 n))))
   ))

(define library (map parse-defn mult-and-neg-deffuns))

(define (fnwae-interp expr)(interp expr library empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; library tests!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; for the record...
; i officially cant stand that we are using 0 as true
; in this language, and 1 as false. 

(test (fnwae-interp (num 5)) 5)
(test (fnwae-interp (parse '(neg? 5))) 1)
(test (fnwae-interp (parse '(neg? 1))) 1)
(test (fnwae-interp (parse '(neg? 0))) 1)
(test (fnwae-interp (parse '(neg? -1))) 0)
(test (fnwae-interp (parse '(neg? -5))) 0)

(test (fnwae-interp (parse '(add-n-times 3 3))) 9)
(test (fnwae-interp (parse '(add-n-times 3 7))) 21)
(test (fnwae-interp (parse '(add-n-times 50 10))) 500)

(test (fnwae-interp (parse '(mult 3 3))) 9)
(test (fnwae-interp (parse '(mult -3 -3))) 9)
(test (fnwae-interp (parse '(mult -3 3))) -9)
(test (fnwae-interp (parse '(mult 3 -3))) -9)
(test (fnwae-interp (parse '(mult 0 0))) 0)
(test (fnwae-interp (parse '(mult 9 0))) 0)
(test (fnwae-interp (parse '(mult 0 9))) 0)
(test (fnwae-interp (parse '(mult -9 0))) 0)
(test (fnwae-interp (parse '(mult 0 -9))) 0)
(test (fnwae-interp (parse '(mult 100 100))) 10000)

(test (fnwae-interp (parse '(pos? 0))) 1)
(test (fnwae-interp (parse '(pos? 1))) 0)
(test (fnwae-interp (parse '(pos? -1))) 1)
(test (fnwae-interp (parse '(pos? 2))) 0)
(test (fnwae-interp (parse '(pos? -2))) 1)

(test (fnwae-interp (parse '(and 1 1))) 1)
(test (fnwae-interp (parse '(and 0 1))) 1)
(test (fnwae-interp (parse '(and 1 0))) 1)
(test (fnwae-interp (parse '(and 0 0))) 0)

(test (fnwae-interp (parse '(or 1 1))) 1)
(test (fnwae-interp (parse '(or 0 1))) 0)
(test (fnwae-interp (parse '(or 1 0))) 0)
(test (fnwae-interp (parse '(or 0 0))) 0)

(test (fnwae-interp (parse '(zero? 1))) 1)
(test (fnwae-interp (parse '(zero? 0))) 0)
(test (fnwae-interp (parse '(zero? -1))) 1)

(test (fnwae-interp (parse '(not 1))) 0)
(test (fnwae-interp (parse '(not 0))) 1)

(test (fnwae-interp (parse '(abs -1))) 1)
(test (fnwae-interp (parse '(abs 1))) 1)
(test (fnwae-interp (parse '(abs 0))) 0)
(test (fnwae-interp (parse '(abs -5))) 5)
(test (fnwae-interp (parse '(abs 5))) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (parse 7) (num 7))
(test (parse 'a) (id 'a))
(test (parse '(+ 6 7)) (add (num 6) (num 7)))
(test (parse '(- 6 7)) (sub (num 6) (num 7)))
(test (parse '(+ 6 (+ 6 7))) (add (num 6) (add (num 6) (num 7))))
(test (parse '(- 6 (- 6 7))) (sub (num 6) (sub (num 6) (num 7))))
(test (parse '(with (x 7) x)) (with 'x (num 7) (id 'x)))
(test (parse '(f x y)) (app 'f (list (id 'x) (id 'y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-defn tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (parse-defn '{deffun {f} x}) (fundef 'f '() (id 'x)))
(test (parse-defn '{deffun {f x} x}) (fundef 'f (list 'x) (id 'x)))
(test (parse-defn '{deffun {f x y} {+ x y}}) 
      (fundef 'f '(x y) (add (id 'x) (id 'y))))
(test (parse-defn '{deffun {f x y} {with {x 7} x}}) 
      (fundef 'f '(x y) (with 'x (num 7) (id 'x))))
(test (parse-defn '{deffun {f x y} {f y x}}) 
      (fundef 'f '(x y) (app 'f (list (id 'y) (id 'x)))))
(test/exn (parse-defn '(deffun (f y y) (+ y 1))) "bad syntax")
(test/exn (parse-defn '(deffun (f y x y) (+ y 1))) "bad syntax")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-fundef tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test/exn (fundef-lookup 'z '()) "undefined function: z")
(test (fundef-lookup 'f (list (fundef 'f (list 'x) (id 'x)))) 
      (fundef 'f (list 'x) (id 'x)))
(test (fundef-lookup 'f (list (fundef 'f '() (id 'x))))(fundef 'f '() (id 'x)))
(test/exn (fundef-lookup 'z (list (fundef 'f (list 'x) (id 'x)))) 
          "undefined function: z")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interpreter tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; base cases
(test (interp (num 5) empty empty) 5)
(test (interp (parse '(+ 1 2)) empty empty) 3)
(test (interp (parse '(- 1 2)) empty empty) -1)
(test (interp (parse '(with (x (+ 1 17)) (+ x 12))) empty empty) 30)
(test (interp (parse '(if0 0 5 6)) empty empty) 5)
(test (interp (parse '(if0 1 5 6)) empty empty) 6)

; application of no arg function
(test 
 (interp (parse '{+ {f} {f}})(list (parse-defn '{deffun {f} 5})) empty) 
 10)
; simple function application
(test (interp (parse '(f 10))
              (list (parse-defn '{deffun {f y} {+ y 1}})) empty) 11)
(test (interp (parse '(f 10))
              (list (parse-defn '{deffun {f y} {with {y 7} y}})) empty) 7)
(test 
 (interp (parse '{f 1}) (list (parse-defn '{deffun {f x} {+ x 8}})) empty)
 9)
(test 
 (interp (parse '{f 1 2}) (list (parse-defn '{deffun {f x y} {+ x y}})) empty)
 3)

; error cases
(test/exn (interp (id 'x)  empty empty) "free identifier")
(test/exn (interp (parse '(f 10)) empty empty) "undefined function: f")
(test/exn 
 (interp (parse '{f 1})(list (parse-defn '{deffun {f} {+ 6 7}})) empty)
 "wrong arity")
(test/exn 
 (interp (parse '{f 1})(list (parse-defn '{deffun {f x y} {+ x y}})) empty)
 "wrong arity")

; make sure we use an empty ds repo when calling no arg function
 (test/exn 
  (interp (parse '{with {x 9} {with {y 10} {f}}}) 
          (list (parse-defn '{deffun {f} {+ x y}})) empty)
 "free identifier")
 
 ; make sure 'with' bindings dont pollute functions 
 (test
  (interp (parse '{with {x 9} {with {y 10} {f 5 2}}}) 
          (list (parse-defn '{deffun {f x y} {+ x y}})) empty) 
  7)
 
 (test
  (interp (parse '{with {a 9} {with {b 10} {with {c 100} {f a b c}}}}) 
          (list (parse-defn '{deffun {f x y z } {+ {+ x y} z }})) empty) 
  119)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random utility stuff 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test ((list-of? symbol?) '(a b c)) #t)
(test ((list-of? symbol?) '(a b "ewrewr")) #f)
(test (findo (lambda (x) (= x 5)) (list 1 2 3 4)) (none))
(test (findo (lambda (x) (= x 5)) (list 1 2 3 4 5)) (some 5))
(test (foldl + 0 '(1 2 3 4 5)) 15)
(test (zip '(1 2 3) '(a b c)) (list (list 1 'a) (list 2 'b) (list 3 'c)))
