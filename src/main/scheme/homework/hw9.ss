#lang plai-typed

(define-type TFAE
  [num (n : number)]
  [bool (b : boolean)]
  [add (l : TFAE) (r : TFAE)]
  [sub (l : TFAE) (r : TFAE)]
  [eql (l : TFAE) (r : TFAE)]
  [id (name : symbol)]
  [ifthenelse (tst : TFAE) (thn : TFAE) (els : TFAE)]
  [fun (args : (listof symbol)) (ts : (listof Type)) (body : TFAE)]
  [app (rator : TFAE) (rands : (listof TFAE))])

(define-type Type 
  [numT]
  [boolT]
  [arrowT (dom : (listof Type)) (codom : Type)])

(define-type Id->Type
  [idtype (id : symbol)(typ : Type)])

(define (type-check-expr [expr : TFAE]) : Type
  (real-type-check-expr expr empty))
  
(define (append [xs : (listof 'a)][ys : (listof 'a)])
  (if (empty? xs) ys (append (rest xs) (cons (first xs) ys))))

(define (lookup-type [x : symbol][tenv : (listof Id->Type)])
  (if (empty? tenv) (error 'lookup "unknown id")
      (type-case Id->Type (first tenv)
        [idtype (id t) 
          (if (equal? id x) 
              t 
              (lookup-type x (rest tenv)))])))

(test (lookup-type 'x (list (idtype 'x (numT)))) (numT))
(test (lookup-type 'x (list (idtype 'x (numT))(idtype 'y (boolT)))) (numT))
(test (lookup-type 'y (list (idtype 'x (numT))(idtype 'y (boolT)))) (boolT))
(test/exn (lookup-type 'y (list (idtype 'x (numT)))) "unknown id")

(define (extend-env [tenv : (listof Id->Type)]
                    [args : (listof symbol)] 
                    [ts : (listof Type)]) : (listof Id->Type)
  (append (map2 idtype args ts) tenv)) 

(test (extend-env (list) (list 'x) (list (numT))) (list (idtype 'x (numT))))

(define (real-type-check-expr [expr : TFAE] [tenv : (listof Id->Type)]) : Type 
  (begin ;(display tenv)
  (type-case TFAE expr
    [num (n) (numT)]
    [bool (b) (boolT)] 
    [id (x) (lookup-type x tenv)]
    [add (l r) (math-type-check l r tenv (numT) 'add)]
    [sub (l r) (math-type-check l r tenv (numT) 'sub)]
    [eql (l r) (math-type-check l r tenv (boolT) 'eql)]
    [ifthenelse (tst t f)
      (type-case Type (real-type-check-expr tst tenv)
        [boolT () 
          (if (equal? (real-type-check-expr t tenv) (real-type-check-expr f tenv))
              (type-check-expr t tenv)
              (error 'if "expected same types in 2nd and 3rd positions"))]
        [else (error 'if "expected boolean in 1st position")])]
    [fun (args typs body)
         ; extend the environment with the types of all the args
         ; type check the body with that new environment
         (arrowT typs (real-type-check-expr body (extend-env tenv args typs)))]
    [app (rator rands)
         (type-case Type (type-check-expr tst)
           [arrowT (arg-types result-type) ...]
           [else (error 'app "app needs function")])])
;    [fun (id body) (closureV id body ds)]
;    [app (fun-expr arg-expr) 
;         (let* ([f (interp fun-expr ds)][a (interp arg-expr ds)])
;           (type-case FAE-Val f
;             [numV (n) (error "application expected procedure")]
;             [closureV (id body cl-ds)
;                       (interp body (cons (symValPair id a) cl-ds))]))
;         ]))

(define (math-type-check [l : TFAE]
                         [r : TFAE]
                         [tenv : (listof Id->Type)]
                         [result : Type]
                         [err : symbol]) : Type
  (type-case Type (real-type-check-expr l tenv)
    [numT () (type-case Type (real-type-check-expr r tenv)
               [numT () result]
               [else (error err "expected number")])]
    [else (error err "expected number")]))

(test (type-check-expr (num 7)) (numT))
(test (type-check-expr (bool #t)) (boolT))
(test (type-check-expr (bool #f)) (boolT))
(test (type-check-expr (add (num 7) (num 7))) (numT))
(test (type-check-expr (sub (num 7) (num 7))) (numT))
(test (type-check-expr (eql (num 7) (num 7))) (boolT))
(test/exn (type-check-expr (add (num 7) (bool #t))) "expected number")
(test/exn (type-check-expr (sub (num 7) (bool #t))) "expected number")
(test/exn (type-check-expr (eql (num 7) (bool #t))) "expected number")

;[fun (args : (listof symbol)) (ts : (listof Type)) (body : TFAE)]
(test (type-check-expr 
       (fun (list 'x) (list (numT)) (add (num 5)(num 5)))) 
      (arrowT (list (numT)) (numT)))

(test (real-type-check-expr (id 'x) (list (idtype 'x (numT)))) (numT))

(test (type-check-expr 
       (fun (list 'x) (list (numT)) (add (id 'x)(num 5)))) 
      (arrowT (list (numT)) (numT)))
