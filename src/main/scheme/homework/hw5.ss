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

(define mymap `{with {mapreduce ,mapreduce} 
                      {fun {f xs} {mapreduce f {fun {x y} {pair x y}} 0 xs}}}) 

(define add2 `{fun {x y} {+ x y}})

;; sum definition for the homework
(define sum `{with {reduce ,reduce} {fun {l} {reduce ,add2 0 l}}})

(define (wrap-with-hw-lib expr)
  `{with {pair ,pair} {with {fst ,fst} {with {snd ,snd} {with {sum ,sum} ,expr}}}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; basics
(test (parse 7) (num 7))
(test (parse 'a) (id 'a))
(test (parse '(+ 6 7)) (add (num 6) (num 7)))
(test (parse '(- 6 7)) (sub (num 6) (num 7)))
(test (parse '(+ 6 (+ 6 7))) (add (num 6) (add (num 6) (num 7))))
(test (parse '(- 6 (- 6 7))) (sub (num 6) (sub (num 6) (num 7))))
(test (parse '(with (x 7) x)) (app (fun 'x (id 'x)) (num 7)))
(test (parse '(if0 0 1 2)) (if0 (num 0) (num 1) (num 2)))

;; application tests
(test/exn (parse-app '(x)) "appliction without arguments")
(test (parse-app '(x y)) (app (id 'x)(id 'y)))
(test (parse-app '(x y z)) (app (app (id 'x)(id 'y)) (id 'z)))
(test (parse-app '(w x y z)) (app (app (app (id 'w)(id 'x)) (id 'y)) (id 'z)))

;; fun tests
(test/exn (parse-fun '(fun () x)) "bad syntax")
(test (parse-fun '(fun (x) x)) (fun 'x (id 'x)))
(test (parse-fun '(fun (x y) x)) (fun 'x (fun 'y (id 'x))))
(test (parse-fun '(fun (x y z) (+ (+ x y) z))) 
      (fun 'x (fun 'y (fun 'z (add (add (id 'x) (id 'y))(id 'z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interp tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; tests not requiring the library functions, just the interpreter
(test (interp (parse 7) '()) (numV 7))
(test (interp (parse (- 10 3)) '()) (numV 7))
(test (interp (parse (+ 3 4)) '()) (numV 7))
(test (interp (parse '(fun (x) (+ x 2))) '()) 
      (closureV 'x (add (id 'x) (num 2)) '()))
(test (interp (parse '((fun (x) (+ x 2)) 5)) '()) (numV 7))
(test (interp (parse '((fun (x) (fun (y) (+ x y))) 5 2)) '()) (numV 7))
(test (interp (parse '((fun (x) (fun (y) (+ x y))) 5)) '()) 
      (closureV 'y (add (id 'x) (id 'y)) (list (symValPair 'x (numV 5)))))

(test (interp (parse '(if0 0 1 2)) '()) (numV 1))

(test (interp-expr (parse 0)) 0)

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

(test (interp-expr (parse (wrap-with-hw-lib '(fst (pair 2 3))))) 2)
(test (interp-expr (parse (wrap-with-hw-lib '(snd (pair 2 0))))) 0)
(test (interp-expr (parse (wrap-with-hw-lib '{snd {pair 1 2}}))) 2)
(test (interp-expr 
       (parse (wrap-with-hw-lib '{snd {fst {pair {pair 1 2} {pair 3 4}}}}))) 2)
(test (interp-expr 
       (parse (wrap-with-hw-lib '{with {p {pair 1 2}}{+ {fst p} {snd p}}}))) 3)
(test (interp-expr 
       (parse (wrap-with-hw-lib 
               '{with {p {pair {fun {x} {+ x 1}} 2}}{{fst p} {snd p}}}))) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sum tests (require hw lib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (interp-expr (parse (wrap-with-hw-lib '(sum 0)))) 0)
(test (interp-expr (parse (wrap-with-hw-lib '(sum (pair 1 0))))) 1)
(test (interp-expr (parse (wrap-with-hw-lib '(sum (pair 2 (pair 1 0)))))) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEYOND HOMEWORK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; add2, and reduce (which in turn includes mapreduce, which includes Y).
;; i would like to have not done that, but instead, would have liked to
;; add those functions to the library directly.
;;
;; by adding them to the library, sum could call reduce and add2
;; without explicitely including it in a with statement, like so:
;; (define sum  `{fun {l} {reduce add2 0 l}})
;; 
;; sum is allowed to call pair, fst, snd implicitely because the HW includes 
;; them in the library
;;
;; there is another way i can add functions to the library.
;; and that is by adding closureV's to the ds repository in interp-expr.
;; this is something i'll try. 
;;

;; redefine library functions without explicit with clauses
(define mapreduce-implicit
  `{Y {fun {mr} 
           {fun {f g id xs} 
                {if0 xs id {g {f {fst xs}} {mr f g id {snd xs}}}}}}})
(define reduce-implicit `{fun {g id xs} {mapreduce {fun {x} x} g id xs}})
(define map-implicit `{fun {f xs} {mapreduce f {fun {x y} {pair x y}} 0 xs}}) 
(define sum-implicit `{fun {l} {reduce add2 0 l}})

(define (wrap-with-fae-lib expr)
  `{with {Y ,Y} 
     {with {pair ,pair} 
       {with {fst ,fst} 
         {with {snd ,snd} 
           {with {mapreduce ,mapreduce-implicit}
             {with {reduce ,reduce-implicit}
               {with {map ,map-implicit}
                 {with {add2 ,add2}
                   {with {sum ,sum-implicit}
                     ,expr}}}}}}}}})

;; this is my second attempt at creating the library
;; these functions get put into the ds repo in fae-ds
;; each closure has a reference to the previous ds repo, 
;; so that each function has a reference to the functions
;; created before it. this is exactly the same as how the
;; 'with' clauses behave in 'wrap-with-lib'
(define (create-lib-func sym body ds)
  (cons (symValPair sym (interp (parse body) ds)) ds))

(define lib 
  (foldl
   (lambda (pair acc-ds)
     (create-lib-func (first pair) (second pair) acc-ds))
   empty
   (list (list 'Y Y)
         (list 'pair pair)
         (list 'fst fst)
         (list 'snd snd)
         (list 'mapreduce mapreduce-implicit)
         (list 'reduce reduce-implicit)
         (list 'map map-implicit)
         (list 'add2 add2)
         (list 'sum sum-implicit))))

;; this function takes some code, gives that code access to the library
;; parses it, and interprets it. 
;; this allows users of the language to call functions in the library
;; naturally, or implicitely. 
(define (fae-wrapped  sexpr) (fae (wrap-with-fae-lib sexpr) '()))

(define (fae-deferred sexpr) (fae sexpr lib))

(define (fae sexpr ds)
  (type-case FAE-Val (interp (parse sexpr) ds)
             [numV (n) n]
             [closureV (s b ds) 'procedure]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; the following test proves that the deferred sub repos
; for both the wrapped version of the fae library (fae-wrapped)
; and the deferred repo version of the library (fae-deferred)
; are identical. 
; parse returns a closure with containing the ds repo
; and this tests checks that those closures are the same. 
;
; this (im quite certain) means that fae-wrapped and fae-deferred
; are identical, and that the duplicate tests below aren't needed
; however, ive decided to keep them there anyway.
(test 
 (interp (parse (wrap-with-fae-lib '{fun {x} x})) '()) 
 (interp (parse '{fun {x} x}) lib)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (fae-deferred '(+ 5 6)) 11)
(test (fae-wrapped '(+ 5 6)) 11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pair tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (fae-wrapped '(fst (pair 2 3))) 2)
(test (fae-deferred '(fst (pair 2 3))) 2)
(test (fae-wrapped '(snd (pair 2 0))) 0)
(test (fae-deferred '(snd (pair 2 0))) 0)
(test (fae-wrapped '{snd {pair 1 2}}) 2)
(test (fae-deferred '{snd {pair 1 2}}) 2)
(test (fae-wrapped '{snd {fst {pair {pair 1 2} {pair 3 4}}}}) 2)
(test (fae-deferred '{snd {fst {pair {pair 1 2} {pair 3 4}}}}) 2)
(test (fae-wrapped '{with {p {pair 1 2}}{+ {fst p} {snd p}}}) 3)
(test (fae-deferred '{with {p {pair 1 2}}{+ {fst p} {snd p}}}) 3)
(test (fae-wrapped '{with {p {pair {fun {x} {+ x 1}} 2}}{{fst p} {snd p}}}) 3)
(test (fae-deferred '{with {p {pair {fun {x} {+ x 1}} 2}}{{fst p} {snd p}}}) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sum tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (fae-wrapped '(sum 0)) 0)
(test (fae-deferred '(sum 0)) 0)
(test (fae-wrapped '(sum (pair 1 0))) 1)
(test (fae-deferred '(sum (pair 1 0))) 1)
(test (fae-wrapped '(sum (pair 2 (pair 1 0)))) 3)
(test (fae-deferred '(sum (pair 2 (pair 1 0)))) 3)