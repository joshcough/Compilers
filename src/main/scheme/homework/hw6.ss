#lang planet plai/plai:1:8
(print-only-errors #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  <RCFAE> = <number>
;          | {+ <RCFAE> <RCFAE>}
;          | {- <RCFAE> <RCFAE>}
;          | {fun {<id>} <RCFAE>}
;          | {<RCFAE> <RCFAE>}             ;; function application
;          | <id>
;          | {with {<id> <RCFAE>} <RCFAE>} ;; shorthand for fun & app
;          | {rec {<id> <RCFAE>}*}
;          | {get <RCFAE> <id>}
;          | {set <RCFAE> <id> <RCFAE>}
;          | {seqn <RCFAE> <RCFAE>}

(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [sub (lhs RCFAE?) (rhs RCFAE?)]
  [id (name symbol?)]
  [if0 (test RCFAE?) (then RCFAE?) (else RCFAE?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (fun RCFAE?) (arg RCFAE?)]
  [rec (fields (listof SymExprPair?))]
  [get (r RCFAE?)(id symbol?)]
  [set (r RCFAE?)(id symbol?)(val RCFAE?)]
  [seqn (a RCFAE?)(b RCFAE?)])

(define-type RCFAE-Val
  [numV (n number?)]
  [closureV (param symbol?)
            (body RCFAE?)
            (ds DefrdSub?)]
  [recV (fields (listof SymValPair?))])

(define-type SymExprPair [symExprPair (name symbol?)(expr RCFAE?)])
(define-type SymValPair [symValPair (name symbol?)(val RCFAE-Val?)])

(define (SymExprPair->SymValPair sep-ds)
  (symValPair (symExprPair-name (first sep-ds)) 
              (interp (symExprPair-expr (first sep-ds)) (second sep-ds))))

(define DefrdSub? (listof SymValPair?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse : sexpr -> RCFAE
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
       [(rec) (parse-rec sexpr)]
       [(get) (get (parse (second sexpr)) (third sexpr))]
       [(set) (set (parse (second sexpr))(third sexpr)(parse (fourth sexpr)))]
       [(seqn)(seqn (parse (second sexpr))(parse (third sexpr)))]
       ;; assume any other symbols are function names, therefore application.
       [else (parse-app sexpr)]
       )]
    [else (error "unexpected token")]))
 

;; parse-with : sexpr -> fun
;;{with {x RCFAE1} RCFAE2} => {{fun {x} RCFAE2} RCFAE1}
(define (parse-with sexpr)
  (app 
   (fun (first (second sexpr)) (parse (third sexpr))) 
   (parse (second (second sexpr)))))

;; parse-app : sexpr -> add
;; {RCFAE1 RCFAE2 RCFAE3 RCFAE4} => {app {app {app 1 2} 3} 4}
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

;; parse-rec : sexpr -> rec
(define (parse-rec sexpr)
  (begin 
    (check-for-dups (map (λ (x) (first x))(cdr sexpr)) "duplicate fields")
    (rec (map (λ (x) (symExprPair (first x)(parse (second x)))) (cdr sexpr)))))
  
(define (check-for-dups l error-message)
  (if (same-size l (remove-duplicates l symbol=?))
      l
      (error error-message)))

;; same-size: list list -> boolean
(define (same-size l r) (= (length l)(length r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interp : RCFAE deferred-subs -> RCFAE-Val
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (interp expr ds)
  (begin (print "interp")
  (type-case RCFAE expr
    [num (n) (numV n)]
    ;; TODO: change + and - to check for functions!
    [add (l r) (addV (interp l ds) (interp r ds))]
    [sub (l r) (subV (interp l ds) (interp r ds))]
    [id (name) (lookup name ds)]
    [if0 (x y z) 
         (if (= 0 (ifop (interp x ds))) (interp y ds) (interp z ds))]
    [fun (id body) (closureV id body ds)]
    [app (fun-expr arg-expr) 
         (let* ([f (interp fun-expr ds)][a (interp arg-expr ds)])
           (type-case RCFAE-Val f
             [closureV (id body cl-ds)
                       (interp body (cons (symValPair id a) cl-ds))]
             [else (error "application expected procedure")]))
         ]
    [rec (fields) 
      (recV (map (lambda (sep) (SymExprPair->SymValPair (list sep ds))) fields))]
    [get (rec id) (find-in-record rec id)] ;; revisit 
    [set (rec id val) (error "implement me")]
    [seqn (a b) (error "implement me")]
    )))

;Provide a definition of interp-expr : RCFAE -> number or 'procedure, as above.
(define (interp-expr expr) 
  (type-case RCFAE-Val (interp expr empty)
    [numV (n) n]
    [closureV (s b ds) 'procedure]
    [recV (l) 'rec]))

(define (addV l r) (mathV + l r))
(define (subV l r) (mathV - l r))
(define (mathV op l r)
  (if 
   (and (numV? l) (numV? r)) 
   (numV (op (numV-n l)(numV-n r)))
   (error "numeric operation expected number")))
(define (ifop n) (if (numV? n) (numV-n n) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup: sym deferred-subs -> RCFAE-Val
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

;; find-in-record : rec sym -> RCFAE
(define (find-in-record rec id)
  (symExprPair-expr 
   (find-or-die 
    (lambda (f) (symbol=? (symExprPair-name f) id)) 
    (rec-fields rec) 
    "no such field")))

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


;; size : any -> number
;; computes a (very rough!) approximate to
;; the size a PLAI object takes in memory
(define (size s)
  (cond
    [(struct? s)
     (size (struct->vector s))]
    [(vector? s)
     (apply +
            (vector-length s)
            (map size (vector->list s)))]
    [(pair? s)
     (+ 1 (size (car s)) (size (cdr s)))]
    [else 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parser tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;; parse basics
;(test (parse 7) (num 7))
;(test (parse 'a) (id 'a))
;(test (parse '(+ 6 7)) (add (num 6) (num 7)))
;(test (parse '(- 6 7)) (sub (num 6) (num 7)))
;(test (parse '(+ 6 (+ 6 7))) (add (num 6) (add (num 6) (num 7))))
;(test (parse '(- 6 (- 6 7))) (sub (num 6) (sub (num 6) (num 7))))
;(test (parse '(with (x 7) x)) (app (fun 'x (id 'x)) (num 7)))
;(test (parse '(if0 0 1 2)) (if0 (num 0) (num 1) (num 2)))
;
;; recs
;(test (parse '(rec (x 6))) (rec (list (symExprPair 'x (num 6)))))
;(test (parse '(rec (x (+ 6 7)))) 
;      (rec (list (symExprPair 'x (add (num 6)(num 7))))))
;(test (parse '(get (rec (x 7)) x)) 
;      (get (rec (list (symExprPair 'x (num 7)))) 'x))
;(test (parse '(get z x)) (get (id 'z) 'x))
;
;; parser allows (+ rec rec), must fail in interpreter
;(test (parse '(+ (rec (x 6)(y 7)) (rec (x 6)(y 7)))) 
;      (add (rec (list (symExprPair 'x (num 6))(symExprPair 'y (num 7))))
;           (rec (list (symExprPair 'x (num 6))(symExprPair 'y (num 7))))))
;
;; parser error case
;(test/exn (parse-rec '{rec {a 0} {a 12}}) "duplicate fields")
;
;;; parse application tests
;(test/exn (parse-app '(x)) "appliction without arguments")
;(test (parse-app '(x y)) (app (id 'x)(id 'y)))
;
;;; parse fun tests
;(test/exn (parse-fun '(fun () x)) "bad syntax")
;(test (parse-fun '(fun (x) x)) (fun 'x (id 'x)))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interp-expr tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;; tests not requiring the library functions, just the interpreter
;(test (interp-expr (parse 0)) 0)
;(test (interp-expr (parse (- 10 3))) 7)
;(test (interp-expr (parse (+ 3 4))) 7)
;(test (interp-expr (parse '((fun (x) (+ x 2)) 5))) 7)
;(test (interp-expr (parse '(((fun (x) (fun (y) (+ x y))) 5 )2))) 7)
;(test (interp-expr (parse '((fun (x) (fun (y) (+ x y))) 5))) 'procedure)
;(test (interp-expr (parse '(fun (x) (+ x 2)))) 'procedure)
;(test (interp-expr (parse '(if0 0 1 2))) 1)
;(test (interp-expr (parse '(if0 1 1 2))) 2)
;
;;; bad math test
;(test/exn (interp-expr (parse '{+ {fun {x} x} 1})) 
;          "numeric operation expected number")
;
;;; bad application test
;(test/exn (interp-expr (parse '{1 2})) "application expected procedure")
;(test/exn (interp-expr (parse '((0 Q) (+ 3 6)))) "free identifier")
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(test (interp-expr (parse '(((fun (x) (fun (x) x)) 5) 6))) 6)

; test for functions with the same id in arg list more than once
;(test (interp-expr (parse '((fun (x x) x) 5 6))) 6)
;(test (interp-expr (parse '((fun (x x x) x) 5 6 7))) 7)
;(test (interp-expr (parse '((fun (x y x) x) 5 6 7))) 7)
;(test (interp-expr (parse '((fun (x x y) x) 5 6 7))) 6)
;(test (interp-expr (parse '((fun (x x y y) y) 5 6 7 8))) 8)


; record
;(test (interp-expr (parse '{rec {a 10} {b {+ 1 2}}})) 'rec)

; get
(test (interp-expr (parse '{get {rec {r 1}} r}))'rec)
;(test (interp-expr (parse '{get {rec {r {rec {z 0}}}} r}))'rec)
;(test (interp-expr (parse '{get {rec {a 10} {b {+ 1 2}}} b})) 3)
;(test (interp-expr 
;       (parse '{+ 10 {get {get {rec {r {rec {z 10}}}} r} z}})) 20)
; nested get
;(test (interp-expr (parse '{get {get {rec {r {rec {z 0}}}} r} z})) 0)
