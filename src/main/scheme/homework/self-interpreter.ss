#lang planet plai/plai:1:14
(print-only-errors #t)

; scheme features used in original fae interpreter
; 
; lists
;; deconstructors - first, second ect
; numbers (and +, -)
; functions
; predicates number? symbol? list?
; symbol=?
; if (only if zero, but that means number comparison)
; let
; define type and its pattern matching and deconstructors
; errors, strings, built in find function, booleans
; just about everything!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Myself
  [num (n number?)]
  [numb? (x Myself?)]
  
  [sym (s symbol?)]
  [symb? (x Myself?)]
  
  [id (name symbol?)]
  
  [add (lhs Myself?) (rhs Myself?)]
  [sub (lhs Myself?) (rhs Myself?)]
  [if0 (test Myself?) (then Myself?) (else Myself?)]
  
  [fun (param symbol?) (body Myself?)]
  [app (fun Myself?) (arg Myself?)]
  
  [my-list (l (listof Myself?))]
  [my-car (l Myself?)]
  [my-cdr (l Myself?)]
  [my-cons (l Myself?) (r Myself?)] 
  [is-list? (x Myself?)]
  [is-empty? (l Myself?)]
  
  ; for debugging
  [my-print (x Myself?)]
  
  ; magic equality test that hopefully returns 0 (true) if
  ; two numbers are the same, or two symbols are the same.....
  [same? (lhs Myself?) (rhs Myself?)])

(define-type Myself-Val
  [numV (n number?)]
  [listV (l (listof Myself-Val?))]
  [symV (s symbol?)]
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
       [(my-list) (my-list (map parse (cdr sexpr)))]
       [(my-car) (my-car (parse (second sexpr)))]
       [(my-cdr) (my-cdr (parse (second sexpr)))]
       [(my-cons) (my-cons (parse (second sexpr)) (parse (third sexpr)))]
       [(sym) (sym (second sexpr))]
       [(same?) (same? (parse (second sexpr)) (parse (third sexpr)))]
       [(numb?) (numb? (parse (second sexpr)))]
       [(symb?) (symb? (parse (second sexpr)))]
       [(is-list?) (is-list? (parse (second sexpr)))]
       [(is-empty?) (is-empty? (parse (second sexpr)))]
       [(my-print) (my-print (parse (second sexpr)))]
       [(fun) (parse-fun sexpr)]
       ;; assume any other symbols are function names, therefore application.
       [else (parse-app sexpr)])]
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
  (begin ;(printf "~s~n" expr)
  (type-case Myself expr
    [my-print (x) 
           (let ([evaled-x (interp x env)]) 
             (begin (printf "PRINT ~s~n" evaled-x) evaled-x))]
    [num (n) (numV n)]
    ; TODO - write tests
    [sym (s) (symV s)]
    [add (l r) (addV (interp l env) (interp r env))]
    [sub (l r) (subV (interp l env) (interp r env))]
    ; TODO - write tests
    [same? (l r) (sameV (interp l env) (interp r env))]
    [id (name) (lookup name env)]
    [if0 (x y z) 
         (if (= 0 (ifop (interp x env))) (interp y env) (interp z env))]
    [fun (id body) (closureV id body env)]
    [app (fun-expr arg-expr) 
         (let* ([f (interp fun-expr env)][a (interp arg-expr env)])
           (type-case Myself-Val f
             [closureV (id body cl-env)
                       (begin
                         ;(printf "-----------------~n")
                         ;(printf "applying (~s/~s~n)" id a)
                         (interp body (cons (symValPair id a) cl-env)))]
             [else (error "application expected procedure")]))]
    [my-list (l) (listV (map (λ (x) (interp x env)) l))]
    [my-cons (x l) 
         (type-case Myself-Val (interp l env)
           [listV (l) (listV (cons (interp x env) l))]
           [else (error "my-car expected list")])]
    [my-car (l) 
         (type-case Myself-Val (interp l env)
           [listV (l) (car l)]
           [else (error "my-car expected list")])]
    [my-cdr (l) 
         (type-case Myself-Val (interp l env)
           [listV (l) (listV (cdr l))]
           [else (error "my-cdr expected list")])]

    ; TODO - write tests
    [numb? (x) 
         (type-case Myself-Val (interp x env)
           [numV (n) (numV 0)] ; returning 0 for true. ick.
           [else (numV 1)])] ; returning 1 for false, double ick!  
    ; TODO - write tests
    [symb? (x) 
         (type-case Myself-Val (interp x env)
           [symV (s) (numV 0)]
           [else (numV 1)])]
    ; TODO - write tests
    [is-list? (x) 
         (type-case Myself-Val (interp x env)
           [listV (l) (numV 0)]
           [else (numV 1)])]
    ; TODO - write tests
    [is-empty? (x) 
         (type-case Myself-Val (interp x env)
           [listV (l) (if (empty? l) (numV 0) (numV 1))]
           [else (numV 1)])]
    )))

(define (interp-expr expr) 
  (type-case Myself-Val (interp expr empty)
    [numV (n) n]
    [symV (s) s]
    [listV (l) l]
    [closureV (a b env) 'procedure]))

(define (addV l r) (mathV + l r))
(define (subV l r) (mathV - l r))
(define (sameV l r) 
  (if (and (numV? l) (numV? r)) 
      (mathV (λ (x y) (if (eq? x y) 0 1)) l r)
      (if (and (symV? l) (symV? r))
          (if (eq? (symV-s l)(symV-s r)) 
              (numV 0)
              (numV 1))
          (numV 1))))

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
;; Y
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
; simple numer case here
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
;; list tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (interp-expr (parse '(my-list 1 5))) (list (numV 1) (numV 5)))
(test (interp-expr (parse '(my-list 1 5 10))) (list (numV 1) (numV 5) (numV 10)))
(test (interp-expr (parse '(my-car (my-list 1 5)))) 1)
(test (interp-expr (parse '(my-car (my-cdr (my-list 1 5))))) 5)
(test (interp-expr (parse '(my-car (my-cdr (my-list (my-list 1 5) (my-list 5 1)))))) (list (numV 5) (numV 1)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boolean library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define boolean-lib
  (create-lib (list base-lib)
   (list
    (list 'or '(fun (x y) (if0 x 0 (if0 y 0 1))))
    (list 'and '(fun (x y) (if0 x (if0 y 0 1) 1)))
    (list 'zero? '(fun (x) (if0 x 0 1)))
    (list 'not '(fun (x) (if0 x 1 0)))
    )))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; math library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define math-lib
  (create-lib (list boolean-lib base-lib) 
   (list
    (list 'addf addf)
    (list 'split-neg? '(Y (fun (SN)(fun (p n) (if0 (or (zero? p)(zero? n)) 1
      (if0 (+ n 1) 0 (if0 (zero? (- p 1)) 1 (SN (- p 1) (+ n 1)))))))))
    (list 'neg? '(fun (x) (split-neg? x x)))
    (list 'pos? '(fun (x) (and (not (zero? x)) (not (neg? x)))))
    (list 'abs '(fun (x) (if0 (neg? x) (- 0 x) x)))
    (list 'add-n-times 
          '(Y (fun (NX) (fun (n x) (if0 n 0 (+ x (NX (- n 1) x)))))))
    (list 'mult '(fun (x y)
      (with (n (add-n-times (abs x) y)) (if0 (pos? x) n (- 0 n)))))
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; option library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define option-lib
  (create-lib (list base-lib)
    (list
     (list 'some `{fun {x} {my-list {sym just} x}})
     (list 'none `{fun {x} {my-list {sym none} x}}) ; weird to use x here at all, but ok for now.
     (list 'is-some? `{fun {x} {if0 {is-list? x} {if0 {same? {sym just} {my-car x}} 0 1} 1}})
     (list 'get `{fun {o} {if0 {is-some? o} {my-car {my-cdr o}} {sym error-get-on-none}}})
     (list 'omap `{fun {f o} {if {is-some? o} {some {f {get o}}} {none 1}}})
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list-lib
  (create-lib (list option-lib math-lib base-lib)
   (list
    (list '1st `(fun (l) (my-car l)))
    (list '2nd `(fun (l) (my-car (my-cdr l))))
    (list '3rd `(fun (l) (my-car (my-cdr (my-cdr l)))))
    (list '4th `(fun (l) (my-car (my-cdr (my-cdr (my-cdr l))))))
    (list '5th `(fun (l) (my-car (my-cdr (my-cdr (my-cdr (my-cdr l))))))) 
    (list 'mapreduce  
          `{Y {fun {MR} 
                   {fun {f g id xs} 
                        {if0 {is-empty? xs} id {g {f {1st xs}} {MR f g id {my-cdr xs}}}}}}})
    (list 'reduce `{fun {g id xs} {mapreduce {fun {x} x} g id xs}})
    (list 'map `{fun {f xs} {mapreduce f {fun {x y} {my-cons x y}} {my-list} xs}})
    (list 'find `{fun {f xs} {mapreduce
                              {fun {x} {if0 {f x} {some x} {none 1}}}
                              ; reduce takes the first value where f(x) is true
                              ; y represents what weve found so far
                              ; x represents the current item. 
                              {fun {x y} {if0 {is-some? y} y {if0 {is-some? x} x y}}}
                              ; id is none
                              {none 1}
                              xs}})
    (list 'sum `{fun {l} {reduce addf 0 l}}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; finally, the entire myself library.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define myself-lib (create-lib (list list-lib option-lib math-lib boolean-lib base-lib) '()))

;; parse the expression, and then interpret (with access to the main library)
(define (myself sexpr) (interp (parse sexpr) myself-lib))

(define (to-plt myself-value)
  (type-case Myself-Val myself-value
             [numV (n) n]
             [symV (s) s]
             [listV (l) l]
             [closureV (s b ds) 'procedure]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (to-plt (myself '(+ 5 6))) 11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (to-plt (myself '(my-car (my-list 2 3)))) 2)
(test (to-plt (myself '(2nd (my-list 2 0)))) 0)
(test (to-plt (myself '{2nd {my-list 1 2}})) 2)
(test (to-plt (myself '{2nd {my-car {my-list {my-list 1 2} {my-list 3 4}}}})) 2)
(test (to-plt (myself '{with {p {my-list 1 2}}{+ {my-car p} {2nd p}}})) 3)
(test (to-plt (myself '{with {p {my-list {fun {x} {+ x 1}} 2}}{{my-car p} {2nd p}}})) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list library tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; sum
(test (to-plt (myself '(sum (my-list)))) 0)
(test (to-plt (myself '(sum (my-list 1 0)))) 1)
(test (to-plt (myself '(sum (my-list 2 1 0)))) 3)
(test (to-plt (myself '(sum (my-list 9 2 1 0)))) 12)
(test (to-plt (myself '(sum (my-list 9 2 1 0)))) 12)
(test (to-plt (myself '(sum (my-list 0 0 0 0 0 0 0 0)))) 0)

; reduce

(test (to-plt (myself `(reduce addf 0 (my-list 0 0 0 0)))) 0)
(test (to-plt (myself `(reduce addf 5 (my-list 10 20 30 40)))) 105)

; map

; since lists are functions, i cant easily test the results of map for equality
; so instead, i map, then sum, then check the answer. 

(test (to-plt (myself `(sum (map (fun (x) x) (my-list))))) 0)

(test (to-plt (myself `(sum (map (fun (x) (+ x 10)) (my-list 0 0 0 0)))))
      40)

(test (to-plt (myself `(same? 5 5))) 0)

; find
(test (to-plt (myself `(find (fun (x) (same? x 5)) (my-list 5 0))))
      (to-plt (myself '(some 5))))

(test (to-plt (myself `(find (fun (x) (same? x 7)) (my-list 5 0))))
      (to-plt (myself '(none 1))))

(test (to-plt (myself `(find (fun (x) (same? x 10)) (my-list 20 10 5))))
      (to-plt (myself '(some 10))))

(test (to-plt (myself `(find (fun (x) (same? x 7)) (my-list 20 10 5))))
      (to-plt (myself '(none 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; math lib tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (to-plt (myself '(neg? 5))) 1)
(test (to-plt (myself '(neg? 1))) 1)
(test (to-plt (myself '(neg? 0))) 1)
(test (to-plt (myself '(neg? -1))) 0)
(test (to-plt (myself '(neg? -5))) 0)
 
(test (to-plt (myself '(add-n-times 3 3))) 9)
(test (to-plt (myself '(add-n-times 3 7))) 21)
(test (to-plt (myself '(add-n-times 50 10))) 500)
 
(test (to-plt (myself '(mult 3 3))) 9)
(test (to-plt (myself '(mult -3 -3))) 9)
(test (to-plt (myself '(mult -3 3))) -9)
(test (to-plt (myself '(mult 3 -3))) -9)
(test (to-plt (myself '(mult 0 0))) 0)
(test (to-plt (myself '(mult 9 0))) 0)
(test (to-plt (myself '(mult 0 9))) 0)
(test (to-plt (myself '(mult -9 0))) 0)
(test (to-plt (myself '(mult 0 -9))) 0)
(test (to-plt (myself '(mult 100 100))) 10000)
 
(test (to-plt (myself '(pos? 0))) 1)
(test (to-plt (myself '(pos? 1))) 0)
(test (to-plt (myself '(pos? -1))) 1)
(test (to-plt (myself '(pos? 2))) 0)
(test (to-plt (myself '(pos? -2))) 1)
 
(test (to-plt (myself '(and 1 1))) 1)
(test (to-plt (myself '(and 0 1))) 1)
(test (to-plt (myself '(and 1 0))) 1)
(test (to-plt (myself '(and 0 0))) 0)
 
(test (to-plt (myself '(or 1 1))) 1)
(test (to-plt (myself '(or 0 1))) 0)
(test (to-plt (myself '(or 1 0))) 0)
(test (to-plt (myself '(or 0 0))) 0)
 
(test (to-plt (myself '(zero? 1))) 1)
(test (to-plt (myself '(zero? 0))) 0)
(test (to-plt (myself '(zero? -1))) 1)
 
(test (to-plt (myself '(not 1))) 0)
(test (to-plt (myself '(not 0))) 1)
 
(test (to-plt (myself '(abs -1))) 1)
(test (to-plt (myself '(abs 1))) 1)
(test (to-plt (myself '(abs 0))) 0)
(test (to-plt (myself '(abs -5))) 5)
(test (to-plt (myself '(abs 5))) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Notes On Parsing

; Notes On Code Representation in Myself

; the first important question is: 
; what representation am i going to parse?
; answer: it can only be things that the language can understand
; in particular, symV, numV, listV, closureV. but, we probably wont pass the parser closures.

; that doesn't really answer the question though, but it gets us closer.
; first lets explore what code looks like in scheme:
; (+ 6 7) is a list containing the symbol '+ and the numbers 6 and 7
; each of those things can be represented in Myself, however a little bit more verbose:
; (my-list (sym +) 6 7)

; this brings up a few questions:
; why cant Myself use exactly the same representation? 
; to answer that question is to really understand how code Myself is interpreted.
; assuming that there exists parse and eval functions in the Myself library,
; then one should call them like so:
; (eval (parse (my-list (sym +) 6 7)))
; but lets first explore they were called like so:
; (eval (parse (+ 6 7)))
; the key here, is remember that this is Myself code that needs to be parsed and 
; evaluated by the original Myself parsed and evaluator. 
; That IS the runtime for Myself.
; We'll call these original functions myself-k1-parse and myself-k1-eval.
; We expand the code, then, to:
; (myself-k1-eval (myself-k1-parse (eval (parse (+ 6 7)))))
; And knowing what we already know about evaluation in myself-k1-eval,
; We can reduce this to (eval (parse (numV 13)))
; I'm not being 100% complete here, but thats because I believe the material is mostly understood.
; this finally answers the question of why we cant use that original representation.
; its not a list! (+ 6 7) is not a list in Myself. its a function call. 
; parse needs to work on lists. the only way to represent lists in myself is with my-list.
; so we go back to the original - (my-list (sym +) 6 7)
; the story is still somewhat incomplete however. 
; the next question concerns the use the sym function. 
; why is it needed? why cant we say (my-list + 6 7)
; the answer lies in the original parser. when it encounters the +
; it sees it as a bare symbol. it treats bare symbols as ids
; and passes myself-k1-eval (id +) in the AST. 
; when myself-k1-eval interprets the list, it interprets each of the items in the list.
; then it comes across (id +), it attempts to look it up in the environment,
; and its not there, and the evaluation fails. 
; when (sym +) is used, the parser parses this as symbol creation, instead of an id. 
; in scheme, its the difference between x and 'x.
; unfortunately Myself doesn't have such an easy syntax. 
; at this point, one might ask, well then how do we represent ids? 
; the answer to this is quite simple. (sym the-id-you-want)
; as long as it appears in the right location in your code, it will properly 
; be parsed as an id. for example: (my-list (sym +) (sym x) (sym y))
; adds the ids x and y. 
; but then how do you represent symbols themselves? 
; to answer that, we need to know about function application. 
; in Myself-K2, function application looks like so:
; (my-list (sym +) (sym x) (sym y)), but we already know that.
; but this is the key to creating symbols. (sym +) is very much like a function application.
; its not technically a function, but you can basically consider it to be. 
; therefore, we use the following syntax to create the symbol 'x:
; (my-list (sym sym) (sym x))
; brutal, i know. 

; last notes on representation:
; we know that instead of (+ 5 6), we use (list (sym +) 5 6)
; one goal of this project was concerned with not changing the representation too dramatically.
; does this qualify as dramatic? yes and no. it should be clear from the explanation above that
; these both have the same meaning (in myself k1 vs myself k2)
; just that the original implementation can rely on schemes reader, and Myself cant.
; this means that, if we did have a more sophisticated parser, its possible that 
; we could get close to the same syntax. however, in order to do that, 
; you'd probably have to add more power to Myself-k1. 
; but the next goal of this project was to determine the smallest set of features
; needed to enable self-interpretation. both goals are very ambiguous, and it seems in fact,
; in direct opposition to one another. to use the same representation you 
; must add features to the language. to get the smallest set of features you 
; must remove features from the language. 
; i think i've found on OK balance between the two opposing forces. 
; the code at times, is quite ugly. but, i've already added more features than I 
; originally anticipated. i think this fits the definition of good compromise:
; both sides are unhappy. :)

; Notes on AST Representation in Myself

; next important question is: what is the AST going to look like?
; fortunately, this is an easier question to answer. 
; the AST must be a legal Myself-Val because it will be the output of the parser.

; (listV ((symV 'num) (numV 7)))

; the first item in the list will indicate the datatype of the second
; for simplicity, i omit the outer list for the actual ast listings below:

; ((symV 'num) (numV 7))               
; ((symV 'sym) (symV 'something))     
; ((symV '+) lhs rhs)               
; ((symV '-) lhs rhs)               
; ((symV 'id) (symV 'something))      
; ((symV 'if) test then else)         
; ((symV 'fun) id body)
; ((symV 'app) f a)
; ((symV 'list) x1 ... xn)            
; ((symV 'my-car) lst)                
; ((symV 'my-cdr) lst)                 
; ((symV 'numb?) x)                   
; ((symV 'symb?) x)                   
; ((symV 'is-list?) x)  

(define eval-lib
  (create-lib (list list-lib option-lib math-lib boolean-lib base-lib)
   (list 
    (list 'parse `{Y {fun {PARSE} {fun {sexpr} 
            {if0 {numb? sexpr} {my-list {sym num} sexpr}
            {if0 {symb? sexpr} {my-list {sym id} sexpr}
            {if0 {is-list? sexpr}
                 {with {op {1st sexpr}}
                 ; sym
                 ; (list (sym sym) (sym x)) -> ((symV 'sym) (symV x))
                 {if0 {same? {sym sym} op}
                      ; TODO - maybe this could check to make sure the 2nd is a symV       
                      {my-list {sym sym} {2nd sexpr}}
                 ; + 
                 ; (list (sym +) (list lhs rhs)) -> ((symV '+) lhs rhs)
                 {if0 {same? {sym +} op}
                      {my-list {sym +} {PARSE {2nd sexpr}} {PARSE {3rd sexpr}}}
                 ; - 
                 ; (list (sym -) (list lhs rhs)) -> ((symV '-) lhs rhs)
                 {if0 {same? {sym -} op}
                      {my-list {sym -} {PARSE {2nd sexpr}} {PARSE {3rd sexpr}}}
                 ; list
                 ; (list (sym list) (list lhs rhs)) -> ((symV 'list) lhs rhs)
                 {if0 {same? {sym my-list} op}
                      {my-cons {sym my-list} {map {fun {x} {PARSE x}} {my-cdr sexpr}}}
                 ; numb? (list (sym numb?) x) -> ((symV 'numb?) x) 
                 {if0 {same? {sym numb?} op}
                      {my-list {sym numb?} {PARSE {2nd sexpr}}}
                 ; symb? (list (sym symb?) x) -> ((symV 'symb?) x) 
                 {if0 {same? {sym symb?} op}
                      {my-list {sym symb?} {PARSE {2nd sexpr}}}
                 ; is-list? (list (sym list?) x) -> ((symV 'list?) x) 
                 {if0 {same? {sym is-list?} op}
                      {my-list {sym is-list?} {PARSE {2nd sexpr}}}
                 ; my-car (list (sym my-car) x) -> ((symV 'my-car) x) 
                 {if0 {same? {sym my-car} op}
                      {my-list {sym my-car} {PARSE {2nd sexpr}}}
                 ; my-cdr (list (sym my-cdr) x) -> ((symV 'my-cdr) x) 
                 {if0 {same? {sym my-cdr} op}
                      {my-list {sym my-cdr} {PARSE {2nd sexpr}}}
                 ; if0 (list (sym if0) test texpr fexpr) -> ((symV 'if0) test then else)
                 {if0 {same? {sym if0} op}
                      {my-list {sym if0}
                               {PARSE {2nd sexpr}}{PARSE {3rd sexpr}}{PARSE {4th sexpr}}}
                 ; function
                 {if0 {same? {sym fun} op}
                      {my-list {sym fun}
                               ; TODO check for symbol in 2nd position
                               {2nd sexpr} {PARSE {3rd sexpr}}}     
                 ; application
                 {my-list {sym app} (PARSE op) {PARSE {2nd sexpr}}}
                 }}}}}}}}}}}}
                 ; not a numb, symb or list, must be a closure
                 {sym parse-error}}
                 {sym parse-error}}
                 {sym parse-error}}
            }}})
    
    (list 'domath `{fun {l r op}
          {if0 {and {numb? l} {numb? r}} {op l r} {sym eval-error-math-expected-numbers}}})
    
    (list 'lookup `{fun {s env} 
                        {with {f {find {fun {x} {same? {my-car x} s}} env}}
                              {if0 {is-some? f} {my-car {my-cdr {get f}}} {sym error-unknown-id}}}})
    
    ; eval
    (list 'real-eval `{Y {fun {EVAL} {fun {expr env}
            {with {type {1st expr}} {with {body {my-cdr expr}}
              ; numbers ((symV 'num) (numV 5)) -> NumV
              {if0 {same? type {sym num}} {1st body}
              ; ids ((symV 'id) (symV 'x)) -> Myself-Val
              {if0 {same? type {sym id}} {lookup {1st body} env}
              ; symbols ((symV 'sym) (symV 'x)) -> SymV
              {if0 {same? type {sym sym}} {1st body}
              ; + ((symV '+) lhs rhs) -> numV
              {if0 {same? type {sym +}}
                   {with {l {EVAL {1st body} env}} {with {r {EVAL {2nd body} env}}
                     {domath l r {fun {x y} {+ x y}}}}}
              ; - ((symV '-) . (lhs . rhs)) -> numV
              {if0 {same? type {sym -}}
                   {with {l {EVAL {1st body} env}} {with {r {EVAL {2nd body} env}}
                     {domath l r {fun {x y} {- x y}}}}}
              ; if0 ((symV 'if0) . (test . (then . else))) -> Myself-Val
              {if0 {same? type {sym if0}}
                   {if0 {EVAL {1st body} env} {EVAL {2nd body} env} {EVAL {3rd body} env}}
              ; my-list ((symV 'my-list) (listV x1 ... xn)) -> listV
              {if0 {same? type {sym my-list}}
                   {map {fun {x} {EVAL x env}} body}
              ; my-car ((symV 'my-car) . x) -> Myself-Val
              {if0 {same? type {sym my-car}}
                   {with {x {EVAL {1st body} env}}
                         {if0 {is-list? x} {1st x} {sym eval-error-1st-expected-list}}}
              ; my-cdr ((symV 'my-cdr) . x) -> Myself-Val
              {if0 {same? type {sym my-cdr}}
                   {with {x {EVAL {1st body} env}}
                         {if0 {is-list? x} {my-cdr x} {sym eval-error-2nd-expected-list}}}
              ; numb? ((symV 'numb?) . x) -> numV (0 or 1)
              {if0 {same? type {sym numb?}} {if0 {numb? {EVAL {1st body} env}} 0 1}
              ; symb? ((symV 'symb?) . x) -> numV (0 or 1)
              {if0 {same? type {sym symb?}} {if0 {symb? {EVAL {1st body} env}} 0 1}
              ; is-list? ((symV 'is-list?) . x) -> numV (0 or 1)
              {if0 {same? type {sym is-list?}} {if0 {is-list? {EVAL {1st body} env}} 0 1}
              ; fun ((symV 'fun) id body)
              {if0 {same? type {sym fun}} {my-list {sym closure} {1st body} {2nd body}}
              ; app ((symV 'app) f a)
              {if0 {same? type {sym app}}
                   ; TODO check that f is a closure.
                   {with {f {EVAL {1st body} env}}
                         {with {a {EVAL {2nd body} env}}
                               {EVAL {3rd f} {my-cons {my-list {2nd f} a} env}}}}
              {sym eval-error}}}}}}}}}}}}}}}}}}}})
    
    (list 'eval `{fun {exp} {real-eval exp {my-list}}})
    )))

(define myself-meta-lib (create-lib (list eval-lib list-lib option-lib math-lib boolean-lib base-lib) '()))
(define (myself-k2 sexpr) (interp (parse sexpr) myself-meta-lib))

; parse a num
(test (myself-k2 '(parse 5)) (listV (list (symV 'num) (numV 5))))
; eval a num
(test (myself-k2 '(eval (parse 5))) (numV 5))

; parse an id
(test (myself-k2 '(parse (sym f))) (listV (list (symV 'id) (symV 'f))))
; eval a id
(test (myself-k2 '(real-eval (parse (sym x)) (my-list (my-list (sym x) 7)))) (numV 7))
; unknown id
(test (myself-k2 '(real-eval (parse (sym x)) (my-list (my-list (sym y) 7)))) (symV 'error-unknown-id))

; parse an sym
(test (myself-k2 '(parse (my-list (sym sym) (sym f)))) (listV (list (symV 'sym) (symV 'f))))
; eval a sym
(test (myself-k2 '(eval (parse (my-list (sym sym) (sym x))))) (symV 'x))

; parse an + expression
(test (myself-k2 '(parse (my-list (sym +) 5 6))) 
      (listV (list (symV '+) 
                   (listV (list (symV 'num) (numV 5)))
                   (listV (list (symV 'num) (numV 6))))))

; eval an + expression
(test (myself-k2 '(eval (parse (my-list (sym +) 5 6)))) (numV 11))

; parse an - expression
(test (myself-k2 '(parse (my-list (sym -) 6 5))) 
      (listV (list (symV '-)
                   (listV (list (symV 'num) (numV 6)))
                   (listV (list (symV 'num) (numV 5))))))
      
; eval a - expression
(test (myself-k2 '(eval (parse (my-list (sym -) 6 5)))) (numV 1))
      
; parse an if expression
(test (myself-k2 '(parse (my-list (sym if0) 0 5 6))) 
      (listV (list (symV 'if0)
                   (listV (list (symV 'num) (numV 0)))
                   (listV (list (symV 'num) (numV 5))) 
                   (listV (list (symV 'num) (numV 6))))))
      
; eval an if expression
(test (myself-k2 '(eval (parse (my-list (sym if0) 0 42 6)))) 
      (numV 42))
(test (myself-k2 '(eval (parse (my-list (sym if0) 1 42 54))))
      (numV 54))

; parse a list expression
(test (myself-k2 '(parse (my-list (sym my-list))))
      (listV (list (symV 'my-list))))

; eval a list expression
(test (myself-k2 '(eval (parse (my-list (sym my-list))))) 
      (listV (list)))

; parse a list expression
(test (myself-k2 '(parse (my-list (sym my-list) 6)))
      (listV (list 
              (symV 'my-list) 
              (listV (list (symV 'num) (numV 6)))
              )))

; eval a list expression
(test (myself-k2 '(eval (parse (my-list (sym my-list) 6))))
      (listV (list (numV 6))))

; parse a list expression
(test (myself-k2 '(parse (my-list (sym my-list) 6 5)))
      (listV (list (symV 'my-list) 
                   (listV (list (symV 'num) (numV 6)))
                   (listV (list (symV 'num) (numV 5))))))

; eval a list expression
(test (myself-k2 '(eval (parse (my-list (sym my-list) 6 5))))
      (listV (list (numV 6) (numV 5))))

;; parse a my-car expression
(test (myself-k2 '(parse (my-list (sym my-car) (my-list (sym my-list) 6 5)))) 
      (listV (list (symV 'my-car)
                   (listV (list (symV 'my-list)
                                (listV (list (symV 'num) (numV 6)))
                                (listV (list (symV 'num) (numV 5))))))))
      
;; eval a my-car expression
(test (myself-k2 '(eval (parse (my-list (sym my-car) (my-list (sym my-list) 6 5))))) 
      (numV 6))

;; parse a my-cdr expression
(test (myself-k2 '(parse (my-list (sym my-cdr) (my-list (sym my-list) 6 5)))) 
      (listV (list (symV 'my-cdr)
                   (listV (list (symV 'my-list)
                                (listV (list (symV 'num) (numV 6)))
                                (listV (list (symV 'num) (numV 5))))))))      
;; eval a my-cdr expression
(test (myself-k2 '(eval (parse (my-list (sym my-cdr) (my-list (sym my-list) 6 5)))))
      (listV (list (numV 5))))

;; parse a numb? expression
(test (myself-k2 '(parse (my-list (sym numb?) 6)))
      (listV (list (symV 'numb?) (listV (list (symV 'num) (numV 6))))))

(test (myself-k2 '(parse (my-list (sym numb?) (sym x))))
      (listV (list (symV 'numb?) (listV (list (symV 'id) (symV 'x))))))

;; eval a numb? expression
(test (myself-k2 '(eval (parse (my-list (sym numb?) 42)))) 
      (numV 0))
(test (myself-k2 '(eval (parse (my-list (sym numb?) (sym x))))) 
      (numV 1))
(test (myself-k2 '(eval (parse (my-list (sym numb?) (my-list (sym sym) (sym x)))))) 
      (numV 1))

;; parse a symb? expression
(test (myself-k2 '(parse (my-list (sym symb?) 6)))
      (listV (list (symV 'symb?) (listV (list (symV 'num) (numV 6))))))

(test (myself-k2 '(parse (my-list (sym symb?) (my-list (sym sym)(sym x)))))
      (listV (list (symV 'symb?) (listV (list (symV 'sym) (symV 'x))))))

;; the code below really means (symb? (list 6 5))
(test (myself-k2 '(parse (my-list (sym symb?) (my-list (sym my-list) 6 5))))
      (listV (list (symV 'symb?) 
                   (listV (list (symV 'my-list) 
                                (listV (list (symV 'num) (numV 6))) 
                                (listV (list (symV 'num) (numV 5))))))))

;; eval a symb? expression
(test (myself-k2 '(eval (parse (my-list (sym symb?) (my-list (sym sym)(sym x)))))) 
      (numV 0))
(test (myself-k2 '(eval (parse (my-list (sym symb?) 42)))) 
      (numV 1))

;; parse a is-list? expression
(test (myself-k2 '(parse (my-list (sym is-list?) 6)))
      (listV (list (symV 'is-list?) (listV (list (symV 'num) (numV 6))))))

(test (myself-k2 '(parse (my-list (sym +) 5 6))) 
      (listV (list (symV '+) 
                   (listV (list (symV 'num) (numV 5)))
                   (listV (list (symV 'num) (numV 6))))))

(test (myself-k2 '(parse (my-list (sym is-list?) (my-list (sym my-list) 6 5))))
      (listV (list (symV 'is-list?) 
                   (listV (list (symV 'my-list) 
                                (listV (list (symV 'num) (numV 6))) 
                                (listV (list (symV 'num) (numV 5))))))))

;; eval a is-list? expression
(test (myself-k2 '(eval (parse (my-list (sym is-list?) (sym x))))) 
      (numV 1))
(test (myself-k2 '(eval (parse (my-list (sym is-list?) (my-list (sym my-list) 6 5))))) 
      (numV 0))

; parse a fun (fun {x} 5}
(test (myself-k2 '(parse (my-list (sym fun) (sym x) 5))) 
       (listV (list (symV 'fun)(symV 'x)(listV (list (symV 'num) (numV 5))))))

; eval a fun (fun {x} 5}
(test (myself-k2 '(eval (parse (my-list (sym fun) (sym x) 5)))) 
       (listV (list (symV 'closure)(symV 'x)(listV (list (symV 'num) (numV 5))))))


; parse a app {{fun {x} x} 7}
(test (myself-k2 '(parse (my-list (my-list (sym fun) (sym x) (sym x)) 7))) 
       (listV (list (symV 'app) 
                    (listV (list (symV 'fun)(symV 'x)(listV (list (symV 'id) (symV 'x)))))
                    (listV (list (symV 'num) (numV 7))))))

; eval a app {{fun {x} x} 7}
(test (myself-k2 '(eval (parse (my-list (my-list (sym fun) (sym x) (sym x)) 7)))) 
       (numV 7))


; NOTES TO SELF
; these notes were taken along the way, and i decided to keep them
; as an example of some of the thought that i had along the way
; and examples of what I learned. 
; however, they are pretty scattered

; can i try to explain to myself how parsing works....?
; myself-k2 first parses the sexpr (parse 5) (using the original parser written in plt scheme)
; this results in: (app the-parse-function (num 5)) 
; this is then passed to interp (the original myself interpreter written in plt scheme)
; this takes a few steps to finish. first, it evaluates the function and arg arguments.
;   (app (clojureV of the-parse-function) (numV 5))
; then it goes ahead and applies the function by interpreting the body with an extended environment.
;   (interp body-of-the-parse-function ('sexpr (numV 5))) ; that last part is the env

; interpret an expr in ast form.
;(test (myself-k2 '(app (eval ((symV 'num) (numV 7)))) (numV 7))

; more notes:
; it might be very useful to have an error functions

; even something as simple as add is changing the representation a lot!

; some very concerning news: if i call eval without the second arg, the program hangs forever!
;(test (myself-k2 '(eval (parse 5))) (numV 5))

; ugh, how do i represent the empty list? just using (pair 0 0) for now. 
; will i need to add empty? to myself?
; also, it would be awful nice to be able to create arbitrary length lists. 
; i thought that pair would be sufficient, but it makes for horrible code. 
; the language needs to have list as a special form 
; (or add varaidic functions, but im not sure i have time to tackle that)
; i could probably just handle that in the parser itself.


;im having problems with id vs sym. for myself to self interpret, it has to recognize symbols, 
;and able to compare them. in particular, it has to be able have a symb? predicate and a same? function.
;symb? would take a myself expression. to interpret symb?, you first have to interpret its argument,
;and then determine if the result is a symV. that means that myself must have symV as a possible return value.
;and that means that the language must be able to create symVs. maybe we add a sym function to do that
;for us. scheme itself differentiates between symbols and ids like so, x is an id, 'x is a symbol. 
;in myself (at least according to the parser), 'x (scheme symbol) is an id. and '(sym x) is a symbol.
;the original parser has the ability to understand scheme symbols. however, the new parser that is written
;in myself does not have the luxury of understanding scheme symbols. it can of course, check to see if
;something is a symV, and treat that symV as an id, the same way the original parser did. 
;so the new parser will simple ask, symb? and if true return an id. but then how do we represent
;and create symbols in the new language, if symbols are to be understood as ids? we had added the sym
;function to the original language... if symb? returns false (And some other simple checks), the parser
;will move on to checking if what it has is a list, and if so, check the first thing in the list. 
;it could have this list (sym (symV 'x)). that is how it knows!