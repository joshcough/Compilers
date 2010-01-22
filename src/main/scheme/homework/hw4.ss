#lang planet plai/plai:1:8

;(require mzlib/trace)
;(define (split-neg? p n) (if (= 0 p) #f (if (= 0 n) #f (if (= 0 (- p 1)) #f (if (= 0 (+ n 1)) #t (split-neg? (- p 1) (+ n 1)))))))
;(define (neg? x) (if (= 0 x) #f (split-neg? x x )))
;(trace split-neg?)

;  <FunDef> = {deffun {<id> <id>*} <F1WAE>}
;  <F1WAE> = <number>
;          | {+ <F1WAE> <F1WAE>}
;          | {- <F1WAE> <F1WAE>}
;          | {with {<id> <F1WAE>} <F1WAE>}
;          | <id>
;          | {<id> <F1WAE>*}
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)(rhs F1WAE?)]
  [sub (lhs F1WAE?)(rhs F1WAE?)]
  [with (name symbol?)(named-expr F1WAE?)(body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?)(args (list-of? F1WAE?))])

(define-type FunDef
  [fundef (name symbol?)
          (arg-names list?)
          (body F1WAE?)])

(define-type SymNumPair [symNumPair (name symbol?)(num number?)])
;(define DefrdSub? 
;(define-type DefrdSub
;  [mtSub]
;  [aSub (name symbol?)(value number?)(ds DefrdSub?)])
  

;; utility
(define (list-of? t) (lambda (l) (andmap t l)))

;; parse : sexpr -> F1WAE
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
       ;; assume any other symbols are function names, therefore application.
       [else (app (first sexpr) (map parse (cdr sexpr)))]
       )]
    [else (error "unexpected token")]))

;; parse-with : sexpr -> F1WAE
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
;  
;;; subst : F1WAE sym F1WAE-Val -> F1WAE
;(define (subst expr sub-id val)
;  (type-case F1WAE expr
;    [num (n) expr]
;    [add (l r) (add (subst l sub-id val)(subst r sub-id val))]
;    [sub (l r) (sub (subst l sub-id val)(subst r sub-id val))]
;    [with (bound-id named-expr body-expr)
;      (with bound-id 
;        (subst named-expr sub-id val)
;        (if (symbol=? bound-id sub-id)
;            body-expr
;            (subst body-expr sub-id val)))]
;    ;; i dont really feel comfortable turning an already evaluated
;    ;; expression back into an F1WAE that needs to be re-evaluated.
;    ;; TODO - maybe i can find a way to avoid this.
;    [id (name) (if (symbol=? name sub-id) (f1wae-val->f1wae val) expr)]
;    [app (fname arg-exprs)
;         (app fname (map (lambda (x) (subst x sub-id val)) arg-exprs))]
;    [rec (fields) 
;      (rec (map (lambda (x) 
;                  (symExprPair (symExprPair-name x)
;                               (subst (symExprPair-expr x) sub-id val))) 
;                fields))]
;    [get (rec id) (get (subst rec sub-id val) id)]
;    ))
;
;;; substN: F1WAE list-of-sym list-of-F1WAE-Val -> F1WAE
;;; for each id in sub-ids, substitute in expr the corresponding val from vals
;;; TODO - maybe this can be changed to use foldl
;(define (subst-N expr sub-ids vals)
;  (cond [(empty? sub-ids) expr]
;        [else (subst-N (subst expr (first sub-ids)(first vals)) 
;                      (cdr sub-ids) (cdr vals))]))
;
;;; f1wae-val->f1wae : f1wae-val -> f1wae
;;; turns an f1wae-val back into an f1wae
;(define (f1wae-val->f1wae v)
;  (type-case F1WAE-Val v
;    [num-val (n) (num n)]
;    [rec-val (r) r]))
;  
;;; math functions
;(define (add-vals left right) (math-with-vals left right +))
;(define (sub-vals left right) (math-with-vals left right -))
;(define (math-with-vals left right op)
;  (if (and (num-val? left)(num-val? right))
;      (num-val (op (num-val-n left) (num-val-n right)))
;      (error "cant do math on records")))
;
;;; find-in-record : rec sym -> F1WAE
;(define (find-in-record rec id)
;  (symExprPair-expr 
;   (find-or-die 
;    (lambda (f) (symbol=? (symExprPair-name f) id)) 
;    (rec-fields rec) 
;    "no such field")))

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
;; interp : F1WAE list-of-FunDef -> F1WAE-Val
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (interp expr defs ds)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l defs ds) (interp r defs ds))]
    [sub (l r) (- (interp l defs ds) (interp r defs ds))]
    [with (bound-id named-expr body-expr)
      (interp body-expr defs 
              (cons (symNumPair bound-id (interp named-expr defs ds)) ds))]
    [id (name) (lookup name ds)]
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
               (error "wrong arity")))
         ]))

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
;;; substitution tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(test (subst (app 'f (list (id 'x) (id 'y))) 'x (num-val 7)) (app 'f (list (num 7) (id 'y))))
;(test (subst (add (num 1) (id 'x)) 'x (num-val 10)) (add (num 1) (num 10)))
;(test (subst (id 'x) 'x (num-val 10)) (num 10))
;(test (subst (id 'y) 'x (num-val 10)) (id 'y))
;(test (subst (sub (id 'x) (num 1)) 'y (num-val 10))(sub (id 'x) (num 1)))
;(test (subst (app 'x (list (num 10))) 'y (num-val 12))(app 'x (list (num 10))))
;(test (subst (app 'x (list (id 'y))) 'y (num-val 12))(app 'x (list (num 12))))
;(test (subst (app 'y (list (num 10))) 'y (num-val 12)) (app 'y (list (num 10))))
;(test (subst (with 'y (num 17) (id 'x)) 'x (num-val 10))(with 'y (num 17) (num 10)))
;(test (subst (with 'y (id 'x) (id 'y)) 'x (num-val 10))(with 'y (num 10) (id 'y)))
;(test (subst (with 'x (id 'y) (id 'x)) 'x (num-val 10))(with 'x (id 'y) (id 'x)))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; subst-N tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(test (subst-N 
;       (app 'f (list (id 'x) (id 'y))) 
;       (list 'x 'y) 
;       (list (num-val 10)(num-val 20)))
;      (app 'f (list (num 10) (num 20))))
;(test (subst-N 
;       (app 'f (list (id 'x) (id 'y))) 
;       (list 'y 'x) ; just reverse the order 
;       (list (num-val 10)(num-val 20)))
;      (app 'f (list (num 20) (num 10))))
;(test (subst-N 
;       (id 'x) 
;       (list 'x 'y)
;       (list (num-val 10)(num-val 20)))
;      (num 10))
;(test (subst-N (id 'y) (list 'x 'y)(list (num-val 10)(num-val 20)))(num 20))
;(test (subst-N (id 'y) (list 'a 'b)(list (num-val 10)(num-val 20)))(id 'y))
;(test (subst-N (num 7) (list 'x 'y)(list (num-val 10)(num-val 20)))(num 7))
;(test (subst-N (add (id 'x)(id 'y)) (list 'x 'y)(list (num-val 10)(num-val 20)))
;      (add (num 10)(num 20)))
;(test (subst-N (with 'y (id 'x) (id 'y)) (list 'y)(list (num-val 10)))
;      (with 'y (id 'x) (id 'y)))

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

