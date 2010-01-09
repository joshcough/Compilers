;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(planet plai/plai:1:6/lang/reader)
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
       [(with) (with (first (second sexpr)) (parse (second (second sexpr))) (parse(third sexpr)) )]
       )]
    [else (error "unexpected token")]
    ))

(define (symbol<? a b) 
  (string<? (symbol->string a) (symbol->string b)))

(define (free-ids wae) 
  (sort (remove-duplicates (free-ids-with-binders wae (list)) symbol=?) symbol<?))

(define (free-ids-with-binders wae binders)
    (let ([l (type-case WAE wae
             [num (n) (list)]
             [add (l r) (list (free-ids-with-binders l binders)
                              (free-ids-with-binders r binders))]
             [sub (l r) (list (free-ids-with-binders l binders)
                              (free-ids-with-binders r binders))]
             [with (name named-expr body) 
                   (list (free-ids-with-binders named-expr binders) 
                         (free-ids-with-binders body (cons name binders)))]
             [id (name) (if (member name binders) (list) (list name))]
             )]) (flatten l)))

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
;; simple sub tests are exactly the same as add (its debatable that they are needed)
(test (free-ids (parse '(sub 7 8))) (list))
(test (free-ids (parse '(sub a 7))) (list 'a))
(test (free-ids (parse '(sub 7 b))) (list 'b))
(test (free-ids (parse '(sub a b))) (list 'a 'b))
(test (free-ids (parse '(sub b a))) (list 'a 'b))
(test (free-ids (parse '(sub z (sub b a)))) (list 'a 'b 'z))
;; simple with tests
(test (free-ids (parse '(with (a 7) a))) (list))
(test (free-ids (parse '(with (a 7) 8))) (list))
(test (free-ids (parse '(with (a 7) b))) (list 'b)) ;; b free in body expr
(test (free-ids (parse '(with (a b) a))) (list 'b)) ;; b free in named expr
(test (free-ids (parse '(with (a b) b))) (list 'b)) ;; b free in named expr and body expr
(test (free-ids (parse '(with (a a) 7))) (list 'a)) ;; a free in named expr, same name as name being bound
(test (free-ids (parse '(with (a a) a))) (list 'a)) ;; a free in named expr and body expr, same name as name being bound
;; nested with tests
(test (free-ids (parse '(with (a (with (b 7) 8)) a))) (list)) ;; nested with expr, no free ids
(test (free-ids (parse '(with (a (with (b 7) a)) a))) (list 'a)) ;; a free in body of inner with expr
(test (free-ids (parse '(with (a (with (b (add c d)) b)) a))) (list 'c 'd)) ;; c and d free in body of inner with expr


