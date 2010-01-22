#lang planet plai/plai:1:8



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
       [(if0) (if0 (parse (second sexpr)) (parse (third sexpr)) (parse (fourth sexpr)))]
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
        [else (cons (const (first ll) (first lr)) (zip-with const (cdr ll) (cdr lr)))]))

(define (zip ll lr) (zip-with list ll lr))
(test (zip '(1 2 3) '(a b c)) (list (list 1 'a) (list 2 'b) (list 3 'c)))

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
              (cons (symNumPair bound-id (interp named-expr defs ds)) ds))]
    [id (name) (lookup name ds)]
    [if0 (x y z) (if (= 0 (interp x defs ds))
                     (interp y defs ds)
                     (interp z defs ds))]
    [app (fname arg-exprs)
         (local [(define f (fundef-lookup fname defs))]
           (if (= (length (fundef-arg-names f))(length arg-exprs))
               (case (length arg-exprs)
                 [(0) (interp (fundef-body f) defs ds)]
                 [else (interp 
                        (fundef-body f)
                        defs
                        (zip-with symNumPair (fundef-arg-names f) 
                             (map (lambda (x) (interp x defs ds)) arg-exprs)))]
                 )
               (error "wrong arity")))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mult-and-neg-deffuns
  (list '(deffun (neg? x) (if0 x 1 (split-neg? x x)))
        '(deffun (split-neg? p n) 
           (if0 p 1 
                (if0 n 1 
                     (if0 (- p 1) 1 
                          (if0 (+ n 1) 0 (split-neg? (- p 1) (+ n 1)))))))
        '(deffun (abs x) (if0 (neg? x) (- 0 x) x))
        '(deffun (opposite-signs x y) (if0 (neg? x) (if0 (neg? y) 1 0) (neg? y)))
        '(deffun (mult-pos x y) (if0 (- x 1) y (+ y (mult-pos (- x 1) y))))
        '(deffun (mult x y) 
           (if0 x 0 
                (if0 y 0 
                     (if0 (opposite-signs x y) 
                          (- 0 (mult-pos (abs x)(abs y))) 
                          (mult-pos (abs x)(abs y))))))
        ))
(define library (map parse-defn mult-and-neg-deffuns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (interp (num 5) library empty) 5)
(test (interp (parse '(neg? 5)) library empty) 1)
(test (interp (parse '(neg? 1)) library empty) 1)
(test (interp (parse '(neg? 0)) library empty) 1)
(test (interp (parse '(neg? -1)) library empty) 0)
(test (interp (parse '(neg? -5)) library empty) 0)

(test (interp (parse '(mult-pos 3 3)) library empty) 9)
(test (interp (parse '(mult-pos 3 7)) library empty) 21)

(test (interp (parse '(opposite-signs 3 3)) library empty) 1)
(test (interp (parse '(opposite-signs -3 -3)) library empty) 1)
(test (interp (parse '(opposite-signs -3 3)) library empty) 0)
(test (interp (parse '(opposite-signs 3 -3)) library empty) 0)

(test (interp (parse '(mult 3 3)) library empty) 9)
(test (interp (parse '(mult -3 -3)) library empty) 9)
(test (interp (parse '(mult -3 3)) library empty) -9)
(test (interp (parse '(mult 3 -3)) library empty) -9)
(test (interp (parse '(mult 0 0)) library empty) 0)
(test (interp (parse '(mult 9 0)) library empty) 0)
(test (interp (parse '(mult 0 9)) library empty) 0)
(test (interp (parse '(mult -9 0)) library empty) 0)
(test (interp (parse '(mult 0 -9)) library empty) 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random utility stuff 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test ((list-of? symbol?) '(a b c)) #t)
(test ((list-of? symbol?) '(a b "ewrewr")) #f)
(test (findo (lambda (x) (= x 5)) (list 1 2 3 4)) (none))
(test (findo (lambda (x) (= x 5)) (list 1 2 3 4 5)) (some 5))
(test (foldl + 0 '(1 2 3 4 5)) 15)

