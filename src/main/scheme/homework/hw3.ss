;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(planet plai/plai:1:7/lang/reader)
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)(rhs F1WAE?)]
  [sub (lhs F1WAE?)(rhs F1WAE?)]
  [with (name symbol?)(named-expr F1WAE?)(body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?)(args list?)])

(define-type FunDef
  [fundef (name symbol?)
          (arg-names list?)
          (body F1WAE?)])

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
       [else (app (first sexpr) (map parse (cdr sexpr)))]
       )]
    [else (error "unexpected token")]
    ))

; '{deffun {f x y} {+ x y}}
(define (parse-defn sexpr)
  (cond
    [(list? sexpr)
     (case (first sexpr)
       [(deffun) (fundef (first (second sexpr)) 
                         (cdr (second sexpr))
                         (parse (third sexpr)))]
       )]
    [else (error "unexpected token")]
    ))

;(define (zip ll lr)
;  (cond [(empty? ll) '()]
;        [(empty? lr) '()]
;        [else (cons (list (first ll) (first lr)) (zip (cdr ll) (cdr lr)))]))

;; interp : F1WAE list-of-FunDef -> num
(define (interp expr defs)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l defs) (interp r defs))]
    [sub (l r) (- (interp l defs) (interp r defs))]
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
         ]))

;; lookup-fundef : sym list-of-FunDef -> FunDef
(define (lookup-fundef fname l)
  (cond [(empty? l) (error "no such function")]
        [else (if (symbol=? fname (fundef-name (first l)))
              (first l) (lookup-fundef fname (rest l)))]))

;; substN: F1WAE list-of-sym list-of-num -> F1WAE
(define (subst-N expr sub-ids vals)
  (cond [(empty? sub-ids) expr]
        [else (subst-N (subst expr (first sub-ids)(first vals)) 
                      (cdr sub-ids) (cdr vals))]))

;; subst : F1WAE sym num -> F1WAE
(define (subst expr sub-id val)
  (type-case F1WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr body-expr)
      (with bound-id 
        (subst named-expr sub-id val)
        (if (symbol=? bound-id sub-id)
            body-expr
            (subst body-expr sub-id val)))]
    [id (name) (if (symbol=? name sub-id)
                   (num val)
                   expr)]
    [app (fname arg-exprs)
         (app fname (map (lambda (x) (subst x sub-id val)) arg-exprs))]))


(test (foldl + 0 '(1 2 3 4 5)) 15)
;(test (zip '(1 2 3) '(a b c)) (list (list 1 'a) (list 2 'b) (list 3 'c)))

;; parser tests
(test (parse 7) (num 7))
(test (parse 'a) (id 'a))
(test (parse-defn '{deffun {f x} x}) (fundef 'f (list 'x) (id 'x)))
(test (parse-defn '{deffun {f x y} {+ x y}}) 
      (fundef 'f '(x y) (add (id 'x) (id 'y))))
(test (parse '(f x y)) (app 'f (list (id 'x) (id 'y))))

;; substitution tests!
(test (subst (app 'f (list (id 'x) (id 'y))) 'x 7) 
      (app 'f (list (num 7) (id 'y))))

;; interpreter tests!
(test 
 (interp (parse '{f 1 2}) (list (parse-defn '{deffun {f x y} {+ x y}})))
 3)

(test 
 (interp (parse '{+ {f} {f}})(list (parse-defn '{deffun {f} 5}))) 
 10)

(test/exn 
 (interp (parse '{f 1})(list (parse-defn '{deffun {f x y} {+ x y}})))
 "wrong arity")
