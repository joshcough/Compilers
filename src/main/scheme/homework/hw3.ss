;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(planet plai/plai:1:7/lang/reader)
;  <FunDef> = {deffun {<id> <id>*} <F1WAE>}
;  <F1WAE> = <number>
;          | {+ <F1WAE> <F1WAE>}
;          | {- <F1WAE> <F1WAE>}
;          | {with {<id> <F1WAE>} <F1WAE>}
;          | <id>
;          | {<id> <F1WAE>*}
;          | {rec {<id> <F1WAE>}*}
;          | {get <F1WAE> <id>
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)(rhs F1WAE?)]
  [sub (lhs F1WAE?)(rhs F1WAE?)]
  [with (name symbol?)(named-expr F1WAE?)(body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?)(args (list-of? F1WAE?))]
  [rec (fields (list-of? SymExprPair?))]
  [get (r F1WAE?)(id symbol?)])

(define (list-of? t) (lambda (l) (andmap t l)))

(define-type FunDef
  [fundef (name symbol?)
          (arg-names list?)
          (body F1WAE?)])

(define-type F1WAE-Val
  [num-val (n number?)]
  [rec-val (r rec?)])

;; ive chosen to represent records as an F1WAE containing 
;; a list of SymExprPair. 
(define-type SymExprPair [symExprPair (name symbol?)(expr F1WAE?)])

;; parse : sexpr -> F1WAE
(define (parse sexpr)
  (cond
    [(number? sexpr) (num sexpr)]
    [(symbol? sexpr) (id sexpr)]
    [(list? sexpr)
     (case (first sexpr)
       [(+) (add (parse (second sexpr)) (parse (third sexpr)))]
       [(-) (sub (parse (second sexpr)) (parse (third sexpr)))]
       [(with) (with (first (second sexpr)) 
                     (parse (second (second sexpr))) 
                     (parse (third sexpr)) )]
       [(deffun) (parse-defn sexpr)]
       [(rec) (rec (map (lambda (x) 
                          (symExprPair (first x)(parse (second x)))) 
                        (cdr sexpr)))]
       [(get) (get (parse (second sexpr)) (third sexpr))]
       [else (app (first sexpr) (map parse (cdr sexpr)))]
       )]
    [else (error "unexpected token")]
    ))

;; parse-defn : sexpr -> FunDef
(define (parse-defn sexpr)
  (define (same-size l r) (= (length l)(length r)))
  (define (parse-arg-names l)
    (if (same-size l (remove-duplicates l symbol=?)) 
        l 
        (error "bad syntax")))
  (cond
    [(list? sexpr)
     (case (first sexpr)
       [(deffun) (fundef (first (second sexpr)) 
                         (parse-arg-names (cdr (second sexpr)))
                         (parse (third sexpr)))]
       )]
    [else (error "unexpected token")]
    ))

;; lookup-fundef : sym list-of-FunDef -> FunDef
(define (lookup-fundef fname l)
  (cond [(empty? l) (error "no such function")]
        [else (if (symbol=? fname (fundef-name (first l)))
              (first l) (lookup-fundef fname (rest l)))]))

;; subst : F1WAE sym F1WAE-Val -> F1WAE
(define (subst expr sub-id val)
  (type-case F1WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)(subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)(subst r sub-id val))]
    [with (bound-id named-expr body-expr)
      (with bound-id 
        (subst named-expr sub-id val)
        (if (symbol=? bound-id sub-id)
            body-expr
            (subst body-expr sub-id val)))]
    ;; i dont really feel comfortable turning an already evaluated
    ;; expression back into an F1WAE that needs to be re-evaluated.
    ;; TODO - maybe i can find a way to avoid this.
    [id (name) (if (symbol=? name sub-id) (f1wae-val->f1wae val) expr)]
    [app (fname arg-exprs)
         (app fname (map (lambda (x) (subst x sub-id val)) arg-exprs))]
    [rec (fields) 
      (rec (map (lambda (x) 
                  (symExprPair (symExprPair-name x)
                               (subst (symExprPair-expr x) sub-id val))) 
                fields))]
    [get (rec id) (get (subst rec sub-id val) id)]
    ))

;; substN: F1WAE list-of-sym list-of-F1WAE-Val -> F1WAE
;; for each id in sub-ids, substitute in expr the corresponding val from vals
;; TODO - maybe this can be changed to use foldl
(define (subst-N expr sub-ids vals)
  (cond [(empty? sub-ids) expr]
        [else (subst-N (subst expr (first sub-ids)(first vals)) 
                      (cdr sub-ids) (cdr vals))]))

;; f1wae-val->f1wae : f1wae-val -> f1wae
;; turns an f1wae-val back into an f1wae
(define (f1wae-val->f1wae v)
  (type-case F1WAE-Val v
    [num-val (n) (num n)]
    [rec-val (r) r]))
  
(define (add-vals left right) (math-with-vals left right +))
(define (sub-vals left right) (math-with-vals left right -))
;; TODO - this can probably be cleaned up.
(define (math-with-vals left right op)
  (type-case F1WAE-Val left
    [num-val (n) 
             (type-case F1WAE-Val right
               [num-val (m) (num-val (op n m))]
               [rec-val (r) (error "cant do math on records")])]
    [rec-val (r) (error "cant do math on records")]))

;; find-in-record : rec sym -> F1WAE
(define (find-in-record rec id)
  (type-case Option 
    (findo (lambda (x) (symbol=? (symExprPair-name x) id)) (rec-fields rec)) 
    [some (v) (symExprPair-expr v)]
    [none () (error (string-append "field not found: " (symbol->string id)))]))

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

;; interp-expr : F1WAE list-of-FunDef -> num or 'record
(define (interp-expr expr defs)
  (type-case F1WAE-Val (interp expr defs)
    [num-val (n) n]
    [rec-val (r) 'record]))

;; interp : F1WAE list-of-FunDef -> F1WAE-Val
(define (interp expr defs)
  (type-case F1WAE expr
    [num (n) (num-val n)]
    [add (l r) (add-vals (interp l defs) (interp r defs))]
    [sub (l r) (sub-vals (interp l defs) (interp r defs))]
    [with (bound-id named-expr body-expr)
      (interp (subst body-expr bound-id (interp named-expr defs)) defs)]
    [id (name) (error 'interp "free variable")]
    [app (fname arg-exprs)
         (local [(define f (lookup-fundef fname defs))]
           (if (= (length (fundef-arg-names f))(length arg-exprs))
               (case (length arg-exprs)
                 [(0) (interp (fundef-body f) defs)]
                 [else (interp 
                        (subst-N (fundef-body f) 
                                 (fundef-arg-names f) 
                                 (map (lambda (x) (interp x defs)) arg-exprs))
                        defs)])
               (error "wrong arity")))
         ]
    [rec (fields) (rec-val expr)]
    [get (rec id) 
         (interp (find-in-record (rec-val-r (interp rec defs)) id) defs)]
    ))

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
(test (parse '(rec (x 6))) (rec (list (symExprPair 'x (num 6)))))
(test (parse '(rec (x (+ 6 7)))) 
      (rec (list (symExprPair 'x (add (num 6)(num 7))))))
(test (parse '(get (rec (x 7)) x)) 
      (get (rec (list (symExprPair 'x (num 7)))) 'x))
(test (parse '(get z x)) (get (id 'z) 'x))
(test (parse '(f x y)) (app 'f (list (id 'x) (id 'y))))
; funny case... parser allows (+ rec rec), must fail in interpreter
(test (parse '(+ (rec (x 6)(y 7)) (rec (x 6)(y 7)))) 
      (add (rec (list (symExprPair 'x (num 6))(symExprPair 'y (num 7))))
           (rec (list (symExprPair 'x (num 6))(symExprPair 'y (num 7))))))

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
;; substitution tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (subst (app 'f (list (id 'x) (id 'y))) 'x (num-val 7)) (app 'f (list (num 7) (id 'y))))
(test (subst (add (num 1) (id 'x)) 'x (num-val 10)) (add (num 1) (num 10)))
(test (subst (id 'x) 'x (num-val 10)) (num 10))
(test (subst (id 'y) 'x (num-val 10)) (id 'y))
(test (subst (sub (id 'x) (num 1)) 'y (num-val 10))(sub (id 'x) (num 1)))
(test (subst (app 'x (list (num 10))) 'y (num-val 12))(app 'x (list (num 10))))
(test (subst (app 'x (list (id 'y))) 'y (num-val 12))(app 'x (list (num 12))))
(test (subst (app 'y (list (num 10))) 'y (num-val 12)) (app 'y (list (num 10))))
(test (subst (with 'y (num 17) (id 'x)) 'x (num-val 10))(with 'y (num 17) (num 10)))
(test (subst (with 'y (id 'x) (id 'y)) 'x (num-val 10))(with 'y (num 10) (id 'y)))
(test (subst (with 'x (id 'y) (id 'x)) 'x (num-val 10))(with 'x (id 'y) (id 'x)))
(test (subst (parse '(rec (x y))) 'y (num-val 6))(rec (list (symExprPair 'x (num 6)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subst-N tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (subst-N 
       (app 'f (list (id 'x) (id 'y))) 
       (list 'x 'y) 
       (list (num-val 10)(num-val 20)))
      (app 'f (list (num 10) (num 20))))
(test (subst-N 
       (app 'f (list (id 'x) (id 'y))) 
       (list 'y 'x) ; just reverse the order 
       (list (num-val 10)(num-val 20)))
      (app 'f (list (num 20) (num 10))))
(test (subst-N 
       (id 'x) 
       (list 'x 'y)
       (list (num-val 10)(num-val 20)))
      (num 10))
(test (subst-N (id 'y) (list 'x 'y)(list (num-val 10)(num-val 20)))(num 20))
(test (subst-N (id 'y) (list 'a 'b)(list (num-val 10)(num-val 20)))(id 'y))
(test (subst-N (num 7) (list 'x 'y)(list (num-val 10)(num-val 20)))(num 7))
(test (subst-N (add (id 'x)(id 'y)) (list 'x 'y)(list (num-val 10)(num-val 20)))
      (add (num 10)(num 20)))
(test (subst-N (with 'y (id 'x) (id 'y)) (list 'y)(list (num-val 10)))
      (with 'y (id 'x) (id 'y)))
(test (subst-N 
       (parse '(rec (x (+ x y)))) 
       (list 'x 'y) 
       (list (num-val 10)(num-val 20)))
      (rec (list (symExprPair 'x (add (num 10)(num 20))))))
(test (subst-N 
       (parse '(get (rec (x (+ x y))) x)) 
       (list 'x 'y) 
       (list (num-val 10)(num-val 20)))
      (get (rec (list (symExprPair 'x (add (num 10)(num 20))))) 'x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-fundef tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test/exn (lookup-fundef 'z '()) "no such function")
(test (lookup-fundef 'f (list (fundef 'f (list 'x) (id 'x)))) 
      (fundef 'f (list 'x) (id 'x)))
(test (lookup-fundef 'f (list (fundef 'f '() (id 'x))))(fundef 'f '() (id 'x)))
(test/exn (lookup-fundef 'z (list (fundef 'f (list 'x) (id 'x)))) 
          "no such function")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find in record tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (find-in-record (rec (list (symExprPair 'x (num 6)))) 'x) (num 6))
(test (find-in-record 
       (rec (list (symExprPair 'x (num 54))(symExprPair 'y (num 42)))) 'x) 
      (num 54))
(test (find-in-record 
       (rec (list (symExprPair 'x (num 54))(symExprPair 'y (num 42)))) 'y) 
      (num 42))
(test/exn  
 (find-in-record (rec (list (symExprPair 'x (num 6)))) 'z) 
 "field not found: z")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interpreter tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (interp-expr (num 5) empty) 5)
(test 
 (interp-expr (parse '{f 1 2}) (list (parse-defn '{deffun {f x y} {+ x y}})))
 3)
(test 
 (interp-expr (parse '{+ {f} {f}})(list (parse-defn '{deffun {f} 5}))) 
 10)
(test/exn 
 (interp-expr (parse '{f 1})(list (parse-defn '{deffun {f x y} {+ x y}})))
 "wrong arity")
(test (interp-expr (parse '(+ 1 2)) empty) 3)
(test (interp-expr (parse '(- 1 2)) empty) -1)
(test (interp-expr (parse '(with (x (+ 1 17)) (+ x 12))) empty) 30)
(test/exn (interp-expr (id 'x) empty) "free variable")
(test/exn (interp-expr (parse '(f 10)) empty) "no such function")
(test (interp-expr (parse '(f 10))
              (list (parse-defn '{deffun {f y} {+ y 1}}))) 11)
(test (interp-expr (parse '(f 10))
              (list (parse-defn '{deffun {f y} {with {y 7} y}}))) 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests from hw page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (interp-expr (parse '{rec {a 10} {b {+ 1 2}}}) empty) 'record)
(test (interp-expr (parse '{get {rec {a 10} {b {+ 1 2}}} b}) empty) 3)
(test/exn (interp-expr (parse '{get {rec {a 10}} b}) empty) "field not found: b")
(test (interp-expr (parse '{g {rec {a 0} {c 12} {b 7}}})
                   (list (parse-defn '{deffun {g r} {get r c}}))) 12)
(test (interp-expr (parse '{get {rec {r {rec {z 0}}}} r}) empty)'record)
(test (interp-expr (parse '{get {get {rec {r {rec {z 0}}}} r} z})empty) 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random utility stuff 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (findo (lambda (x) (= x 5)) (list 1 2 3 4)) (none))
(test (findo (lambda (x) (= x 5)) (list 1 2 3 4 5)) (some 5))
(test (foldl + 0 '(1 2 3 4 5)) 15)