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

(define (type-check-expr [expr : TFAE]) : Type 
  (type-case TFAE expr
    [num (n) (numT)]
    [bool (b) (boolT)] 
    [add (l r) (math-type-check l r (numT) 'add)]
    [sub (l r) (math-type-check l r (numT) 'sub)]
    [eql (l r) (math-type-check l r (boolT) 'eql)]
    [ifthenelse (tst t f)
      (type-case Type (type-check-expr tst)
        [boolT () 
          (if (equal? (type-check-expr t) (type-check-expr f))
              (type-check-expr t)
              (error 'if "expected same types in 2nd and 3rd positions"))]
        [else (error 'if "expected boolean in 1st position")])]
    [fun (args typ body) (error 'fun "implement me")]
    [app (rator rands) (error 'fun "implement me")]
    [else (error 'type-check-expr "wtfmf")]))
;    [fun (id body) (closureV id body ds)]
;    [app (fun-expr arg-expr) 
;         (let* ([f (interp fun-expr ds)][a (interp arg-expr ds)])
;           (type-case FAE-Val f
;             [numV (n) (error "application expected procedure")]
;             [closureV (id body cl-ds)
;                       (interp body (cons (symValPair id a) cl-ds))]))
;         ]))

(define (math-type-check [l : TFAE][r : TFAE][result : Type][err : symbol]) : Type
  (type-case Type (type-check-expr l)
    [numT () (type-case Type (type-check-expr r)
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