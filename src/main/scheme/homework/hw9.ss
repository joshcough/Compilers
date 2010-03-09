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
  (if (empty? tenv) (error 'lookup "type-error unknown id")
      (type-case Id->Type (first tenv)
        [idtype (id t) 
          (if (equal? id x) 
              t 
              (lookup-type x (rest tenv)))])))

; extend the type environment
(define (extend-env [tenv : (listof Id->Type)]
                    [args : (listof symbol)] 
                    [ts : (listof Type)]) : (listof Id->Type)
  (append (map2 idtype args ts) tenv)) 

(define (real-type-check-expr [expr : TFAE] [tenv : (listof Id->Type)]) : Type 
  (begin ;(display tenv)
  (type-case TFAE expr
    [num (n) (numT)]
    [bool (b) (boolT)] 
    [id (x) (lookup-type x tenv)]
    [add (l r) (math-type-check l r tenv (numT) 'add)]
    [sub (l r) (math-type-check l r tenv (numT) 'sub)]
    [eql (l r) (math-type-check l r tenv (boolT) 'eql)]
    [ifthenelse (tst texp fexp)
      (type-case Type (real-type-check-expr tst tenv)
        [boolT ()
         (local [(define t-type (real-type-check-expr texp tenv))]
          (if (equal? t-type (real-type-check-expr fexp tenv))
            t-type
            (error 'if "type-error expected same types in 2nd and 3rd positions")))]
        [else (error 'if "type-error expected boolean in 1st position")])]
    [fun (args typs body)
         ; extend the environment with the types of all the args
         ; type check the body with that new environment
         (arrowT typs (real-type-check-expr body (extend-env tenv args typs)))]
    [app (rator rands)
         (type-case Type (real-type-check-expr rator tenv)
           [arrowT (arg-types result-type)
             ; type check the rands
             ; make sure they are the same type as arg-types
             ; if so, return result-type
             (local
               [(define rand-types 
                  (map (lambda (rand) (real-type-check-expr rand tenv)) rands))]
               (if (equal? arg-types rand-types) 
                   result-type
                   ; TODO: check one at a time and give a much better error.
                   (error 'app "type-error argument types dont match functions arg types")))]
           [else (error 'app "type-error expected function in 1st position")])])))

(define (math-type-check [l : TFAE]
                         [r : TFAE]
                         [tenv : (listof Id->Type)]
                         [result : Type]
                         [err : symbol]) : Type
  (type-case Type (real-type-check-expr l tenv)
    [numT () (type-case Type (real-type-check-expr r tenv)
               [numT () result]
               [else (error err "type-error expected number in 2nd position")])]
    [else (error err "type-error expected number in 1st position")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; test lookup
(test (lookup-type 'x (list (idtype 'x (numT)))) (numT))
(test (lookup-type 'x (list (idtype 'x (numT))(idtype 'y (boolT)))) (numT))
(test (lookup-type 'y (list (idtype 'x (numT))(idtype 'y (boolT)))) (boolT))
(test/exn (lookup-type 'y (list (idtype 'x (numT)))) "type-error unknown id")

; test extend env 
(test (extend-env (list) (list 'x) (list (numT))) (list (idtype 'x (numT))))

; test prims
;  [num (n : number)]
;  [bool (b : boolean)]
(test (type-check-expr (num 7)) (numT))
(test (type-check-expr (bool #t)) (boolT))
(test (type-check-expr (bool #f)) (boolT))

; test id
;  [id (name : symbol)]
(test (real-type-check-expr (id 'x) (list (idtype 'x (numT)))) (numT))
(test/exn 
 (real-type-check-expr (id 'x) (list (idtype 'y (numT)))) 
 "type-error unknown id")

; test eql
;  [eql (l : TFAE) (r : TFAE)]
(test (type-check-expr (eql (num 7)(num 8))) (boolT))
(test/exn (type-check-expr (eql (bool #t)(num 8))) "eql: type-error expected number")

; test ifthenelse
;  [ifthenelse (tst : TFAE) (thn : TFAE) (els : TFAE)]
(test (type-check-expr (ifthenelse (bool #t) (num 8)(num 9))) (numT))
(test (type-check-expr (ifthenelse (bool #f) (num 8)(num 9))) (numT))
(test/exn 
 (type-check-expr (ifthenelse (num 7) (num 8)(num 9))) 
 "if: type-error expected boolean in 1st position")
(test/exn 
 (type-check-expr (ifthenelse (bool #t) (num 8)(bool #f)))
 "type-error expected same types in 2nd and 3rd positions")

; test math
;  [add (l : TFAE) (r : TFAE)]
;  [sub (l : TFAE) (r : TFAE)]
(test (type-check-expr (add (num 7) (num 7))) (numT))
(test (type-check-expr (sub (num 7) (num 7))) (numT))
(test/exn (type-check-expr (add (bool #t) (num 7))) "add: type-error expected number")
(test/exn (type-check-expr (sub (bool #t) (num 7))) "sub: type-error expected number")
(test/exn (type-check-expr (add (num 7) (bool #t))) "add: type-error expected number")
(test/exn (type-check-expr (sub (num 7) (bool #t))) "sub: type-error expected number")

; test functions
;  [fun (args : (listof symbol)) (ts : (listof Type)) (body : TFAE)]
(test (type-check-expr 
       (fun (list 'x) (list (numT)) (add (num 5)(num 5)))) 
      (arrowT (list (numT)) (numT)))

(test (type-check-expr 
       (fun (list 'x) (list (numT)) (add (id 'x)(num 5)))) 
      (arrowT (list (numT)) (numT)))

; test application
;   [app (rator : TFAE) (rands : (listof TFAE))])
(test (type-check-expr 
       (app (fun (list 'x) (list (numT)) (add (id 'x)(num 5))) (list (num 7)))) 
      (numT))

; two arguments
(test (type-check-expr 
       (app (fun (list 'x 'y) (list (numT) (boolT)) 
                 (ifthenelse (id 'y) (add (id 'x)(num 5)) (num 5))) (list (num 7) (bool #t)))) 
      (numT))

(test/exn 
 (type-check-expr (app (num 8) (list (num 8))))       
 "type-error expected function in 1st position")

(test/exn 
 (type-check-expr
  ; pass a bool to a function wanting a num
  (app (fun (list 'x) (list (numT)) (add (id 'x)(num 5))) (list (bool #t))))
 "app: type-error argument types dont match functions arg types")

; other tests
(test/exn 
 (type-check-expr (ifthenelse (eql (bool #t)(num 8)) (num 8)(num 9))) 
 "eql: type-error expected number")