#lang planet plai/plai:1:14
(print-only-errors #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Myself
  [num (n number?)]
  [add (lhs Myself?) (rhs Myself?)]
  [sub (lhs Myself?) (rhs Myself?)]
  [id (name symbol?)]
  [if0 (test Myself?) (then Myself?) (else Myself?)]
  [fun (param symbol?) (body Myself?)]
  [app (fun Myself?) (arg Myself?)]
  [pair (lhs Myself?) (rhs Myself?)]
  [fst (lst Myself?)] ; first
  [snd (lst Myself?)] ; rest
  [numb? (x Myself?)]
  [pear? (x Myself?)]
  [proc? (x Myself?)]
  )

(define-type Myself-Val
  [numV (n number?)]
  [pairV (p cons?)]
  [closureV (param symbol?)
            (body Myself?)
            (env Env?)])

(define-type SymValPair [symValPair (name symbol?)(val Myself-Val?)])
(define Env? (listof SymValPair?))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse : sexpr -> Myself
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parse sexpr)
  (cond
    [(number? sexpr) (num sexpr)]
    [(symbol? sexpr) (id sexpr)]
    [(list? sexpr)
     (case (first sexpr)
       [(+) (add (parse (second sexpr)) (parse (third sexpr)))]
       [(-) (sub (parse (second sexpr)) (parse (third sexpr)))]
       [(with) (parse-with sexpr)]
       [(if0) (if0 (parse (second sexpr))
                   (parse (third sexpr))
                   (parse (fourth sexpr)))]
       [(pair) (pair (parse (second sexpr))(parse (third sexpr)))]
       [(fst) (fst (parse (second sexpr)))]
       [(snd) (snd (parse (second sexpr)))]
       [(numb?) (numb? (parse (second sexpr)))]
       [(pair?) (pair? (parse (second sexpr)))]
       [(proc?) (proc? (parse (second sexpr)))]
       [(fun) (parse-fun sexpr)]
       ;; assume any other symbols are function names, therefore application.
       [else (parse-app sexpr)]
       )]
    [else (error "unexpected token")]))
 
;; parse-with : sexpr -> fun
;;{with {x myself1} myself2} => {{fun {x} myself2} myself1}
(define (parse-with sexpr)
  (app 
   (fun (first (second sexpr)) (parse (third sexpr))) 
   (parse (second (second sexpr)))))

;; parse-app : sexpr -> add
;; {myself1 myself2 myself3 myself4} => {app {app {app 1 2} 3} 4}
(define (parse-app sexpr)
  (define (helper sexpr) 
    (case (length sexpr)
      [(1) (parse (first sexpr))]
      [else (app (helper (drop-right sexpr 1)) (parse (last sexpr)))]
      )
    )
  ;; if the list is one long...F
  (if (= 1 (length sexpr)) 
      (error "appliction without arguments")
      (helper sexpr)))
  
;{fun {a b} {+ a b}} => {fun {a} {fun {b} {+ a b}}}
;; parse-fun : sexpr -> fun
(define (parse-fun sexpr)
 (define (helper args body)
    (case (length args)
      [(0) (error "bad syntax")]
      [(1) (fun (first args) (parse body))]
      [else (fun (first args) (helper (cdr args) body))]
      )
    )
  (helper (second sexpr) (third sexpr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interp : Myself Env -> Myself-Val
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (interp expr env)
  (type-case Myself expr
    [num (n) (numV n)]
    [add (l r) (addV (interp l env) (interp r env))]
    [sub (l r) (subV (interp l env) (interp r env))]
    [id (name) (lookup name env)]
    [if0 (x y z) 
         (if (= 0 (ifop (interp x env))) (interp y env) (interp z env))]
    [fun (id body) (closureV id body env)]
    [app (fun-expr arg-expr) 
         (let* ([f (interp fun-expr env)][a (interp arg-expr env)])
           (type-case Myself-Val f
             [closureV (id body cl-env)
                       (interp body (cons (symValPair id a) cl-env))]
             [else (error "application expected procedure")]))]
    [fst (l) 
         (type-case Myself-Val (interp l env)
           [pairV (l) (car l)]
           [else (error "fst expected list")])]
    [snd (l) 
         (type-case Myself-Val (interp l env)
           [pairV (l) (cdr l)]
           [else (error "snd expected list")])]
    [pair (l r) (pairV (cons (interp l env) (interp r env)))]
    [numb? (x) 
         (type-case Myself-Val (interp x env)
           [numV (n) (numV 0)] ; returning 0 for true. ick.
           [else (numV 1)])] ; returning 1 for false, double ick!  
    [pear? (x) 
         (type-case Myself-Val (interp x env)
           [pairV (l) (numV 0)]
           [else (numV 1)])]
    [proc? (x) 
         (type-case Myself-Val (interp x env)
           [closureV (a b env) (numV 0)]
           [else (numV 1)])]
    ))

(define (interp-expr expr) 
  (type-case Myself-Val (interp expr empty)
    [numV (n) n]
    [pairV (l) l]
    [closureV (a b env) 'procedure]))

(define (addV l r) (mathV + l r))
(define (subV l r) (mathV - l r))
(define (mathV op l r)
  (if 
   (and (numV? l) (numV? r)) 
   (numV (op (numV-n l)(numV-n r)))
   (error "numeric operation expected number")))
(define (ifop n) (if (numV? n) (numV-n n) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup: sym env -> Myself-Val
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (lookup name ds)
  (symValPair-val (find-or-die
    (λ (f) (symbol=? (symValPair-name f) name)) ds 
    (string-append "free identifier: " (symbol->string name)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; utility types and functions
;; Scala's Option, Maybe in Haskell.
(define-type Option [none] [some (v (λ (x) #t))])
  
(define (findo p l)
  (cond [(empty? l) (none)]
        [(p (first l)) (some (first l))]
        [else (findo p (cdr l))]))
;; find-or-die: predicate list[T] string -> T
(define (find-or-die p l error-string)
  (type-case Option (findo p l) [some (v) v] [none () (error error-string)]))

(define (zip-with const ll lr)
  (cond [(empty? ll) '()]
        [(empty? lr) '()]
        [else (cons (const (first ll) (first lr))
                    (zip-with const (cdr ll) (cdr lr)))]))
 
(define (zip ll lr) (zip-with list ll lr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Y combinator
(define Y
  `(fun (X)
    ((fun (p) (X (fun (arg) ((p p) arg))))
     (fun (p) (X (fun (arg) ((p p) arg)))))))

(define addf `{fun {x y} {+ x y}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse basics
(test (parse 7) (num 7))
(test (parse 'a) (id 'a))
(test (parse '(+ 6 7)) (add (num 6) (num 7)))
(test (parse '(- 6 7)) (sub (num 6) (num 7)))
(test (parse '(+ 6 (+ 6 7))) (add (num 6) (add (num 6) (num 7))))
(test (parse '(- 6 (- 6 7))) (sub (num 6) (sub (num 6) (num 7))))
(test (parse '(with (x 7) x)) (app (fun 'x (id 'x)) (num 7)))
(test (parse '(if0 0 1 2)) (if0 (num 0) (num 1) (num 2)))

;; parse application tests
(test/exn (parse-app '(x)) "appliction without arguments")
(test (parse-app '(x y)) (app (id 'x)(id 'y)))
(test (parse-app '(x y z)) (app (app (id 'x)(id 'y)) (id 'z)))
(test (parse-app '(w x y z)) (app (app (app (id 'w)(id 'x)) (id 'y)) (id 'z)))
(test (parse-app '(0 Q (+ 3 6))) (app (app (num 0) (id 'Q)) (add (num 3) (num 6))))

;; parse fun tests
(test/exn (parse-fun '(fun () x)) "bad syntax")
(test (parse-fun '(fun (x) x)) (fun 'x (id 'x)))
(test (parse-fun '(fun (x y) x)) (fun 'x (fun 'y (id 'x))))
(test (parse-fun '(fun (x x) x)) (fun 'x (fun 'x (id 'x))))
(test (parse-fun '(fun (x y z) (+ (+ x y) z))) 
      (fun 'x (fun 'y (fun 'z (add (add (id 'x) (id 'y))(id 'z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interp tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; simple tests just to make sure interp returns myself-vals
; simple number case here
(test (interp (parse 7) '()) (numV 7))
; at least make sure we return simple closures
; this would the place to put tests for more complicated
; closures, if any errors arise. i haven't had any.
(test (interp (parse '(fun (x) (+ x 2))) '()) 
      (closureV 'x (add (id 'x) (num 2)) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interp-expr tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tests not requiring the library functions, just the interpreter
(test (interp-expr (parse 0)) 0)
(test (interp-expr (parse (- 10 3))) 7)
(test (interp-expr (parse (+ 3 4))) 7)
(test (interp-expr (parse '((fun (x) (+ x 2)) 5))) 7)
(test (interp-expr (parse '((fun (x) (fun (y) (+ x y))) 5 2))) 7)
(test (interp-expr (parse '((fun (x) (fun (y) (+ x y))) 5))) 'procedure)
(test (interp-expr (parse '(fun (x) (+ x 2)))) 'procedure)
(test (interp-expr (parse '(if0 0 1 2))) 1)
(test (interp-expr (parse '(if0 1 1 2))) 2)

;; bad math test
(test/exn (interp-expr (parse '{+ {fun {x} x} 1})) 
          "numeric operation expected number")

;; bad application test
(test/exn (interp-expr (parse '{1 2})) "application expected procedure")
(test/exn (interp-expr (parse '(0 f A 2 Y 0 w A))) "free identifier") 
(test/exn (interp-expr (parse '(0 Q (+ 3 6)))) "free identifier")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (interp-expr (parse '((fun (x) (fun (x) x)) 5 6))) 6)

; test for functions with the same id in arg list more than once
(test (interp-expr (parse '((fun (x x) x) 5 6))) 6)
(test (interp-expr (parse '((fun (x x x) x) 5 6 7))) 7)
(test (interp-expr (parse '((fun (x y x) x) 5 6 7))) 7)
(test (interp-expr (parse '((fun (x x y) x) 5 6 7))) 6)
(test (interp-expr (parse '((fun (x x y y) y) 5 6 7 8))) 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pair tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (interp-expr (parse '(pair 1 5))) (cons (numV 1) (numV 5)))
(test (interp-expr (parse '(fst (pair 1 5)))) 1)
(test (interp-expr (parse '(snd (pair 1 5)))) 5)
(test (interp-expr (parse '(snd (pair (pair 1 5) (pair 5 1))))) (cons (numV 5) (numV 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (create-lib depends-on-libs pairs)
  (define (create-lib-func sym body ds)
    (cons (symValPair sym (interp (parse body) ds)) ds))
  (foldl
   (lambda (pair acc-ds)
     (create-lib-func (first pair) (second pair) acc-ds))
   (foldl append '() depends-on-libs)
   pairs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define base-lib 
  (create-lib '() (list (list 'Y Y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; boolean library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define boolean-lib
;  (create-lib (list base-lib)
;   (list
;    (list 'or '(fun (x y) (if0 x 0 (if0 y 0 1))))
;    (list 'and '(fun (x y) (if0 x (if0 y 0 1) 1)))
;    (list 'zero? '(fun (x) (if0 x 0 1)))
;    (list 'not '(fun (x) (if0 x 1 0)))
;    )))
;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; math library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define math-lib
;  (create-lib (list boolean-lib base-lib) 
;   (list
;    (list 'addf addf)
;    (list 'split-neg? '(Y (fun (SN)(fun (p n) (if0 (or (zero? p)(zero? n)) 1
;      (if0 (+ n 1) 0 (if0 (zero? (- p 1)) 1 (SN (- p 1) (+ n 1)))))))))
;    (list 'neg? '(fun (x) (split-neg? x x)))
;    (list 'pos? '(fun (x) (and (not (zero? x)) (not (neg? x)))))
;    (list 'abs '(fun (x) (if0 (neg? x) (- 0 x) x)))
;    (list 'add-n-times 
;          '(Y (fun (NX) (fun (n x) (if0 n 0 (+ x (NX (- n 1) x)))))))
;    (list 'mult '(fun (x y)
;      (with (n (add-n-times (abs x) y)) (if0 (pos? x) n (- 0 n)))))
;    )))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(define list-lib
;  (create-lib (list math-lib base-lib)
;   (list 
;    (list 'mapreduce  
;          `{Y {fun {MR} 
;                   {fun {f g id xs} 
;                        {if0 xs id {g {f {fst xs}} {MR f g id {snd xs}}}}}}})
;    (list 'reduce `{fun {g id xs} {mapreduce {fun {x} x} g id xs}})
;    (list 'map `{fun {f xs} {mapreduce f {fun {x y} {pair x y}} 0 xs}})
;    (list 'sum `{fun {l} {reduce addf 0 l}}))))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finally, the entire myself library.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(define myself-lib (create-lib (list list-lib math-lib boolean-lib base-lib) '()))
;
;;; parse the expression, and then interpret (with access to the main library)
;(define (myself sexpr)
;  (type-case Myself-Val (interp (parse sexpr) myself-lib)
;             [numV (n) n]
;             [pairV (l) l]
;             [closureV (s b ds) 'procedure]))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simple tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(test (myself '(+ 5 6)) 11)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pair tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(test (myself '(fst (pair 2 3))) 2)
;(test (myself '(snd (pair 2 0))) 0)
;(test (myself '{snd {pair 1 2}}) 2)
;(test (myself '{snd {fst {pair {pair 1 2} {pair 3 4}}}}) 2)
;(test (myself '{with {p {pair 1 2}}{+ {fst p} {snd p}}}) 3)
;(test (myself '{with {p {pair {fun {x} {+ x 1}} 2}}{{fst p} {snd p}}}) 3)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list library tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;; sum
;(test (myself '(sum 0)) 0)
;(test (myself '(sum (pair 1 0))) 1)
;(test (myself '(sum (pair 2 (pair 1 0)))) 3)
;(test (myself '(sum (pair 9 (pair 2 (pair 1 0))))) 12)
;(test (myself '(sum (pair 0 (pair 9 (pair 2 (pair 1 0)))))) 12)
;(test (myself '(sum (pair 0 (pair 0 (pair 0 (pair 0 0)))))) 0)
;
;; reduce
;
;(test (myself `(reduce addf 0 (pair 0 (pair 0 (pair 0 (pair 0 0)))))) 0)
;(test (myself 
;       `(reduce addf 5 (pair 10 (pair 20 (pair 30 (pair 40 0)))))) 105)
;
;; map
;
;; since lists are functions, i cant easily test the results of map for equality
;; so instead, i map, then sum, then check the answer. 
;
;(test (myself 
;       `(sum
;         (map (fun (x) (+ x 10)) (pair 0 (pair 0 (pair 0 (pair 0 0)))))))
;      40)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; math lib tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(test (myself '(neg? 5)) 1)
;(test (myself '(neg? 1)) 1)
;(test (myself '(neg? 0)) 1)
;(test (myself '(neg? -1)) 0)
;(test (myself '(neg? -5)) 0)
; 
;(test (myself '(add-n-times 3 3)) 9)
;(test (myself '(add-n-times 3 7)) 21)
;(test (myself '(add-n-times 50 10)) 500)
; 
;(test (myself '(mult 3 3)) 9)
;(test (myself '(mult -3 -3)) 9)
;(test (myself '(mult -3 3)) -9)
;(test (myself '(mult 3 -3)) -9)
;(test (myself '(mult 0 0)) 0)
;(test (myself '(mult 9 0)) 0)
;(test (myself '(mult 0 9)) 0)
;(test (myself '(mult -9 0)) 0)
;(test (myself '(mult 0 -9)) 0)
;(test (myself '(mult 100 100)) 10000)
; 
;(test (myself '(pos? 0)) 1)
;(test (myself '(pos? 1)) 0)
;(test (myself '(pos? -1)) 1)
;(test (myself '(pos? 2)) 0)
;(test (myself '(pos? -2)) 1)
; 
;(test (myself '(and 1 1)) 1)
;(test (myself '(and 0 1)) 1)
;(test (myself '(and 1 0)) 1)
;(test (myself '(and 0 0)) 0)
; 
;(test (myself '(or 1 1)) 1)
;(test (myself '(or 0 1)) 0)
;(test (myself '(or 1 0)) 0)
;(test (myself '(or 0 0)) 0)
; 
;(test (myself '(zero? 1)) 1)
;(test (myself '(zero? 0)) 0)
;(test (myself '(zero? -1)) 1)
; 
;(test (myself '(not 1)) 0)
;(test (myself '(not 0)) 1)
; 
;(test (myself '(abs -1)) 1)
;(test (myself '(abs 1)) 1)
;(test (myself '(abs 0)) 0)
;(test (myself '(abs -5)) 5)
;(test (myself '(abs 5)) 5)