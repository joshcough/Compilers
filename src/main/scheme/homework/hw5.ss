#lang planet plai/plai:1:8
(print-only-errors #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [sub (lhs FAE?) (rhs FAE?)]
  [id (name symbol?)]
  [if0 (test FAE?) (then FAE?) (else FAE?)]
  [fun (param symbol?) (body FAE?)]
  [app (fun FAE?) (arg FAE?)])

(define-type FAE-Val
  [numV (n number?)]
  [closureV (param symbol?)
            (body FAE?)
            (ds DefrdSub?)])

(define-type SymValPair [symValPair (name symbol?)(val FAE-Val?)])
(define DefrdSub? (listof SymValPair?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse : sexpr -> FnWAE
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
       [(fun) (parse-fun sexpr)]
       ;; assume any other symbols are function names, therefore application.
       [else (parse-app sexpr)]
       )]
    [else (error "unexpected token")]))
 

;; parse-with : sexpr -> fun
;;{with {x FAE1} FAE2} => {{fun {x} FAE2} FAE1}
(define (parse-with sexpr)
  (app 
   (fun (first (second sexpr)) (parse (third sexpr))) 
   (parse (second (second sexpr)))))

;; parse-app : sexpr -> add
;; {FAE1 FAE2 FAE3 FAE4} => {app {app {app 1 2} 3} 4}
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
;; interp : FAE deferred-subs -> FAE-Val
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (interp expr ds)
  (type-case FAE expr
    [num (n) (numV n)]
    ;; TODO: change + and - to check for functions!
    [add (l r) (addV (interp l ds) (interp r ds))]
    [sub (l r) (subV (interp l ds) (interp r ds))]
    [id (name) (lookup name ds)]
    [if0 (x y z) 
         (if (= 0 (ifop (interp x ds))) (interp y ds) (interp z ds))]
    [fun (id body) (closureV id body ds)]
    [app (fun-expr arg-expr) 
         (type-case FAE-Val (interp fun-expr ds)
           [numV (n) (error "application expected procedure")]
           [closureV (id body cl-ds)
                     (interp body (cons (symValPair id (interp arg-expr ds)) cl-ds))])
                     ]))
 
;Provide a definition of interp-expr : FAE -> number or 'procedure, as above.
(define (interp-expr expr) 
  (type-case FAE-Val (interp expr empty)
    [numV (n) n]
    [closureV (s b ds) 'procedure]))

(define (addV l r) (mathV + l r))
(define (subV l r) (mathV - l r))
(define (mathV op l r)
  (if 
   (and (numV? l) (numV? r)) 
   (numV (op (numV-n l)(numV-n r)))
   (error "numeric operation expected number")))
(define (ifop n) (if (numV? n) (numV-n n) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup: sym deferred-subs -> FAE-Val
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

; primitive functions
(define pair `{fun {l r} {fun {b} {b l r}}})
(define fst  `{fun {p} {with {true {fun {x y} x}}{p true}}})
(define snd  `{fun {p} {with {false {fun {x y} y}}{p false}}})

; Y combinator
(define Y
  `(fun (X)
    ((fun (p) (X (fun (arg) ((p p) arg))))
     (fun (p) (X (fun (arg) ((p p) arg)))))))

;; guy steele's defs from: 
;; http://groups.csail.mit.edu/mac/users/gjs/6.945/readings/MITApril2009Steele.pdf
;(define (mapreduce f g id xs)
;  (cond ((null? xs) id)
;        (else (g (f (car xs)) (mapreduce f g id (cdr xs))))))
;(define (map f xs) (mapreduce (λ (x) (list (f x))) append '() xs))
;(define (reduce g id xs) (mapreduce (λ (x) x) g id xs))

(define mapreduce 
  `{with {Y ,Y} 
         {Y {fun {mr} 
                 {fun {f g id xs} 
                      {if0 xs id 
                           {g {f {fst xs}} {mr f g id {snd xs}}}}}}}})

(define reduce `{with {mapreduce ,mapreduce} 
                      {fun {g id xs} {mapreduce {fun {x} x} g id xs}}})  

(define addf `{fun {x y} {+ x y}})

;; sum definition for the homework
(define sum `{with {reduce ,reduce} {fun {l} {reduce ,addf 0 l}}})

(define (wrap-with-hw-lib expr)
  `{with {pair ,pair} 
         {with {fst ,fst} 
               {with {snd ,snd} 
                     {with {sum ,sum} ,expr}}}})

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
 
;; simple tests just to make sure interp returns FAE-vals
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
;; pair tests (require hw lib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hw-interp sexp) (interp-expr (parse (wrap-with-hw-lib sexp))))

(test (hw-interp '(fst (pair 2 3))) 2)
(test (hw-interp '(snd (pair 2 0))) 0)
(test (hw-interp '{snd {pair 1 2}}) 2)
(test (hw-interp '{snd {fst {pair {pair 1 2} {pair 3 4}}}}) 2)
(test (hw-interp '{with {p {pair 1 2}}{+ {fst p} {snd p}}}) 3)
(test (hw-interp '{with {p {pair {fun {x} {+ x 1}} 2}}{{fst p} {snd p}}}) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sum tests (require hw lib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (hw-interp '(sum 0)) 0)
(test (hw-interp '(sum (pair 1 0))) 1)
(test (hw-interp '(sum (pair 2 (pair 1 0)))) 3)
(test (hw-interp '(sum (pair 9 (pair 2 (pair 1 0))))) 12)
(test (hw-interp '(sum (pair 0 (pair 9 (pair 2 (pair 1 0)))))) 12)
(test (hw-interp '(sum (pair 0 (pair 0 (pair 0 (pair 0 0)))))) 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scope games using hw lib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (hw-interp '(with (sum 5) sum)) 5)
(test (hw-interp '((with (sum (fun (x) (+ x 5))) sum) 7)) 12)
(test (hw-interp 
       '(with (sum (fun (l) (+ 5 (sum l)))) 
              (sum (pair 0 (pair 0 (pair 0 (pair 0 0))))))) 
      5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEYOND HOMEWORK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; everything below this point is above what was required for the homework
;; and therefore you need not read it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some important comments about this...
;;
;; this line was taken from the homework:
;; {with {pair ...} {with {fst ...} {with {snd ...} {with {sum ...} {sum 0}}}}}
;;
;; always wrapping an expression in the above definitions means that those
;; definitions are 'the library' since the homework does this automatically, 
;; its not easy to add more functions to the library. 
;;
;; to satisfy the requirements for the homework, sum explicitely includes 
;; addf, and reduce (which in turn includes mapreduce, which includes Y).
;; 
;; i would like to have not done that, but instead, would have liked to
;; add those functions to the library directly.
;;
;; by adding them to the library, sum could call reduce and addf
;; without explicitely including it in a with statement, like so:
;; (define sum  `{fun {l} {reduce addf 0 l}})
;; 
;; sum is allowed to call pair, fst, snd implicitely because the HW includes 
;; them in the library
;;
;;
;; below is my attempt at creating that library.
;; these functions get put into the ds repo in fae.
;; each closure has a reference to the previous ds repo, 
;; so that each function has a reference to the functions
;; created before it. 
;;
;; it very clear that a particular function
;; DOESNT depend on a function declared before it. 
;;
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
  (create-lib '()
   (list (list 'Y Y)
         (list 'pair pair)
         (list 'fst fst)
         (list 'snd snd))))

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
;; list library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list-lib
  (create-lib (list math-lib base-lib)
   (list 
    (list 'mapreduce  
          `{Y {fun {MR} 
                   {fun {f g id xs} 
                        {if0 xs id {g {f {fst xs}} {MR f g id {snd xs}}}}}}})
    (list 'reduce `{fun {g id xs} {mapreduce {fun {x} x} g id xs}})
    (list 'map `{fun {f xs} {mapreduce f {fun {x y} {pair x y}} 0 xs}})
    (list 'sum `{fun {l} {reduce addf 0 l}}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; finally, the entire fae library.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fae-lib (create-lib (list list-lib math-lib boolean-lib base-lib) '()))

;; parse the expression, and then interpret (with access to the main library)
(define (fae sexpr)
  (type-case FAE-Val (interp (parse sexpr) fae-lib)
             [numV (n) n]
             [closureV (s b ds) 'procedure]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (fae '(+ 5 6)) 11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pair tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (fae '(fst (pair 2 3))) 2)
(test (fae '(snd (pair 2 0))) 0)
(test (fae '{snd {pair 1 2}}) 2)
(test (fae '{snd {fst {pair {pair 1 2} {pair 3 4}}}}) 2)
(test (fae '{with {p {pair 1 2}}{+ {fst p} {snd p}}}) 3)
(test (fae '{with {p {pair {fun {x} {+ x 1}} 2}}{{fst p} {snd p}}}) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list library tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; sum
(test (fae '(sum 0)) 0)
(test (fae '(sum (pair 1 0))) 1)
(test (fae '(sum (pair 2 (pair 1 0)))) 3)
(test (fae '(sum (pair 9 (pair 2 (pair 1 0))))) 12)
(test (fae '(sum (pair 0 (pair 9 (pair 2 (pair 1 0)))))) 12)
(test (fae '(sum (pair 0 (pair 0 (pair 0 (pair 0 0)))))) 0)

; reduce

(test (fae `(reduce addf 0 (pair 0 (pair 0 (pair 0 (pair 0 0)))))) 0)
(test (fae 
       `(reduce addf 5 (pair 10 (pair 20 (pair 30 (pair 40 0)))))) 105)

; map

; since lists are functions, i cant easily test the results of map for equality
; so instead, i map, then sum, then check the answer. 

(test (fae 
       `(sum
         (map (fun (x) (+ x 10)) (pair 0 (pair 0 (pair 0 (pair 0 0)))))))
      40)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; math lib tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (fae '(neg? 5)) 1)
(test (fae '(neg? 1)) 1)
(test (fae '(neg? 0)) 1)
(test (fae '(neg? -1)) 0)
(test (fae '(neg? -5)) 0)
 
(test (fae '(add-n-times 3 3)) 9)
(test (fae '(add-n-times 3 7)) 21)
(test (fae '(add-n-times 50 10)) 500)
 
(test (fae '(mult 3 3)) 9)
(test (fae '(mult -3 -3)) 9)
(test (fae '(mult -3 3)) -9)
(test (fae '(mult 3 -3)) -9)
(test (fae '(mult 0 0)) 0)
(test (fae '(mult 9 0)) 0)
(test (fae '(mult 0 9)) 0)
(test (fae '(mult -9 0)) 0)
(test (fae '(mult 0 -9)) 0)
(test (fae '(mult 100 100)) 10000)
 
(test (fae '(pos? 0)) 1)
(test (fae '(pos? 1)) 0)
(test (fae '(pos? -1)) 1)
(test (fae '(pos? 2)) 0)
(test (fae '(pos? -2)) 1)
 
(test (fae '(and 1 1)) 1)
(test (fae '(and 0 1)) 1)
(test (fae '(and 1 0)) 1)
(test (fae '(and 0 0)) 0)
 
(test (fae '(or 1 1)) 1)
(test (fae '(or 0 1)) 0)
(test (fae '(or 1 0)) 0)
(test (fae '(or 0 0)) 0)
 
(test (fae '(zero? 1)) 1)
(test (fae '(zero? 0)) 0)
(test (fae '(zero? -1)) 1)
 
(test (fae '(not 1)) 0)
(test (fae '(not 0)) 1)
 
(test (fae '(abs -1)) 1)
(test (fae '(abs 1)) 1)
(test (fae '(abs 0)) 0)
(test (fae '(abs -5)) 5)
(test (fae '(abs 5)) 5)