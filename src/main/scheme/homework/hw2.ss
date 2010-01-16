#lang planet plai/plai:1
(define-type WAE
  [num (n number?)]
  [add (lhs WAE?)(rhs WAE?)]
  [sub (lhs WAE?)(rhs WAE?)]
  [with (name symbol?)(named-expr WAE?)(body WAE?)]
  [id (name symbol?)])

(define (parse sexpr)
  (cond
    [(number? sexpr) (num sexpr)]
    [(symbol? sexpr) (id sexpr)]
    [(list? sexpr)
     (case (first sexpr)
       [(add) (add (parse (second sexpr)) (parse (third sexpr)))]
       [(sub) (sub (parse (second sexpr)) (parse (third sexpr)))]
       [(with) (with (first (second sexpr)) 
                     (parse (second (second sexpr))) 
                     (parse(third sexpr)) )]
       )]
    [else (error "unexpected token")]
    ))

(define (symbol<? a b) 
  (string<? (symbol->string a) (symbol->string b)))


(define (free-ids wae) (free-ids-with-binders wae (list)))

(define (free-ids-with-binders wae binders)
    (let ([l (type-case WAE wae
             [num (n) (list)]
             [id (name) (if (member name binders) (list) (list name))]
             [add (l r) (list (free-ids-with-binders l binders)
                              (free-ids-with-binders r binders))]
             [sub (l r) (list (free-ids-with-binders l binders)
                              (free-ids-with-binders r binders))]
             [with (name named-expr body) 
                   (list (free-ids-with-binders named-expr binders) 
                         (free-ids-with-binders body (cons name binders)))]
             )]) (sort (remove-duplicates (flatten l) symbol=?) symbol<?)))


(define (binding-ids wae)
    (let ([l (type-case WAE wae
             [num (n) (list)]
             [id (name) (list)]
             [add (l r) (list (binding-ids l) (binding-ids r))]
             [sub (l r) (list (binding-ids l) (binding-ids r))]
             [with (name named-expr body) 
                   (list name 
                         (binding-ids named-expr) 
                         (binding-ids body))]
             )]) (sort (remove-duplicates (flatten l) symbol=?) symbol<?)))


(define (bound-ids wae) (bound-ids-with-binders wae (list)))

(define (bound-ids-with-binders wae binders)
    (let ([l (type-case WAE wae
             [num (n) (list)]
             [id (name) (if (member name binders) (list name) (list))]
             [add (l r) (list (bound-ids-with-binders l binders)
                              (bound-ids-with-binders r binders))]
             [sub (l r) (list (bound-ids-with-binders l binders)
                              (bound-ids-with-binders r binders))]
             [with (name named-expr body) 
                   (list (bound-ids-with-binders named-expr binders) 
                         (bound-ids-with-binders body (cons name binders)))]
             )]) (sort (remove-duplicates (flatten l) symbol=?) symbol<?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test cases for free-ids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; trivial cases
(test (free-ids (parse 7)) (list))
(test (free-ids (parse 'a)) (list 'a))

;; simple add tests
(test (free-ids (parse '(add 7 8))) (list))
(test (free-ids (parse '(add a 7))) (list 'a))
(test (free-ids (parse '(add 7 b))) (list 'b))
(test (free-ids (parse '(add a b))) (list 'a 'b))
(test (free-ids (parse '(add b a))) (list 'a 'b)) ;; change the order
(test (free-ids (parse '(add z (add b a)))) (list 'a 'b 'z))

;; simple sub tests are exactly the same as add 
;;(its debatable that they are needed)
(test (free-ids (parse '(sub 7 8))) (list))
(test (free-ids (parse '(sub a 7))) (list 'a))
(test (free-ids (parse '(sub 7 b))) (list 'b))
(test (free-ids (parse '(sub a b))) (list 'a 'b))
(test (free-ids (parse '(sub b a))) (list 'a 'b))
(test (free-ids (parse '(sub z (sub b a)))) (list 'a 'b 'z))

;; simple with tests
(test (free-ids (parse '(with (a 7) a))) (list))
(test (free-ids (parse '(with (a 7) 8))) (list))
;; b free in body expr
(test (free-ids (parse '(with (a 7) b))) (list 'b)) 
;; b free in named expr
(test (free-ids (parse '(with (a b) a))) (list 'b)) 
;; b free in named expr and body expr
(test (free-ids (parse '(with (a b) b))) (list 'b)) 
;; a free in named expr, same name as name being bound
(test (free-ids (parse '(with (a a) 7))) (list 'a)) 
;; a free in named expr same name as name being bound, bound body expr
(test (free-ids (parse '(with (a a) a))) (list 'a)) 

;; nested with tests
;; nested with expr, no free ids
(test (free-ids (parse '(with (a (with (b 7) 8)) a))) (list)) 
;; a free in body of inner with expr
(test (free-ids (parse '(with (a (with (b 7) a)) a))) (list 'a))
;; c and d free in named expr of inner with expr
(test (free-ids (parse '(with (a (with (b (add c d)) b)) a))) (list 'c 'd)) 
;;;;;;;;;;add a few more tests here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test cases for binding-ids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; trivial cases
(test (binding-ids (parse 7)) (list))
(test (binding-ids (parse 'a)) (list))

;; simple add tests - no binding ids at all, because no with expressions
(test (binding-ids (parse '(add 7 8))) (list))
(test (binding-ids (parse '(add a 7))) (list))
(test (binding-ids (parse '(add 7 b))) (list))
(test (binding-ids (parse '(add a b))) (list))
(test (binding-ids (parse '(add b a))) (list))
(test (binding-ids (parse '(add z (add b a)))) (list))

;; simple with tests
(test (binding-ids (parse '(with (a 6) b))) (list 'a))
(test (binding-ids (parse '(with (a a) b))) (list 'a))
(test (binding-ids (parse '(with (a b) a))) (list 'a))
(test (binding-ids (parse '(with (a a) a))) (list 'a))
(test (binding-ids (parse '(with (a 7) a))) (list 'a))
(test (binding-ids (parse '(with (a 7) 8))) (list 'a))
(test (binding-ids (parse '(with (a b) b))) (list 'a))
(test (binding-ids (parse '(with (a a) 7))) (list 'a))
(test (binding-ids (parse '(with (a a) a))) (list 'a))

;; nested with tests
(test (binding-ids (parse '(with (a (with (b 7) 8)) a))) (list 'a 'b))
(test (binding-ids (parse '(with (a (with (b 7) a)) a))) (list 'a 'b)) 
;;;;;;;;;;add a few more tests here

;; add tests with inner with expressions
(test (binding-ids (parse '(add 7 (with (a 6) b)))) (list 'a))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test cases for bound-ids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; trivial cases
(test (bound-ids (parse 7)) (list))
(test (bound-ids (parse 'a)) (list))

;; simple add tests
(test (bound-ids (parse '(add 7 8))) (list))
(test (bound-ids (parse '(add a 7))) (list))
(test (bound-ids (parse '(add 7 b))) (list))
(test (bound-ids (parse '(add a b))) (list))
(test (bound-ids (parse '(add b a))) (list))
(test (bound-ids (parse '(add z (add b a)))) (list))

;; simple with tests
(test (bound-ids (parse '(with (a 7) a))) (list 'a))
(test (bound-ids (parse '(with (a 7) 8))) (list))
;; b free in body expr
(test (bound-ids (parse '(with (a 7) b))) (list)) 
;; b free in named expr
(test (bound-ids (parse '(with (a b) a))) (list 'a)) 
;; b free in named expr and body expr
(test (bound-ids (parse '(with (a b) b))) (list)) 
;; a free in named expr, same name as name being bound
(test (bound-ids (parse '(with (a a) 7))) (list))
 ;; a free in named expr same name as name being bound, bound body expr
(test (bound-ids (parse '(with (a a) a))) (list 'a))

;; nested with tests
;; nested with expr, no free ids
(test (bound-ids (parse '(with (a (with (b 7) 8)) a))) (list 'a)) 
;; a free in body of inner with expr
(test (bound-ids (parse '(with (a (with (b 7) a)) a))) (list 'a)) 
;; c and d free in named expr of inner with expr
(test (bound-ids (parse '(with (a (with (b (add c d)) b)) a))) (list 'a 'b)) 
(test 
 (bound-ids (parse '(with (a (with (b (add c d)) b)) (with (z a) (add z z))))) 
 (list 'a 'b 'z))
(test 
 (bound-ids (parse '(with (z (with (a (add c d)) b)) (with (b a) (add z z))))) 
 (list 'z))
(test 
 (bound-ids (parse '(with (z (with (z (add c d)) b)) (with (z a) (add z z))))) 
 (list 'z))
;;;;;;;;;;add a few more tests here
(test (bound-ids (parse '(with (x 1) 1))) (list))
