#lang plai-typed

; expressions
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

; types
(define-type Type 
  [numT]
  [boolT]
  [arrowT (dom : (listof Type)) (codom : Type)])

; the type environment
(define-type Id->Type
  [idtype (id : symbol)(typ : Type)])

; lookup a type in the type environment
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

; concatenate two lists.
(define (append [xs : (listof 'a)][ys : (listof 'a)])
  (if (empty? xs) ys (append (rest xs) (cons (first xs) ys))))

; public function
(define (type-check-expr [expr : TFAE]) : Type
  (real-type-check-expr expr empty))

; function doing the real work
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
      (local [(define tst-type (real-type-check-expr tst tenv))]          
        (type-case Type tst-type
          [boolT ()
           (local [(define t-type (real-type-check-expr texp tenv))]
             (local [(define f-type (real-type-check-expr fexp tenv))]
               (if (equal? t-type f-type)
                 t-type
                 (error 'if (build-if-error-message t-type f-type)))))]
          [else (error 'if (build-positional-error (boolT) 1 tst-type))]))]
    [fun (args typs body)
         ; extend the environment with the types of all the args
         ; type check the body with that new environment
         (arrowT typs (real-type-check-expr body (extend-env tenv args typs)))]
    [app (rator rands)
         (local [(define rator-type (real-type-check-expr rator tenv))]
           (type-case Type rator-type
             [arrowT (arg-types result-type)
               ; type check the rands
               ; make sure they are the same type as arg-types
               ; if so, return result-type
               (local
                 [(define rand-types 
                    (map (lambda (rand) (real-type-check-expr rand tenv)) rands))]
                 (if (equal? arg-types rand-types) 
                     result-type
                     (error 'app (build-app-mismatch-error arg-types rand-types))))]
             [else (error 'app (build-positional-error-s "function" 1 rator-type))]))])))

(define (math-type-check [l : TFAE]
                         [r : TFAE]
                         [tenv : (listof Id->Type)]
                         [result : Type]
                         [err : symbol]) : Type
  (local [(define l-type (real-type-check-expr l tenv))]
    (local [(define r-type (real-type-check-expr r tenv))]
      (type-case Type l-type
        [numT () 
          (type-case Type r-type
            [numT () result]
            [else (error err (build-positional-error (numT) 2 r-type))])]
        [else (error err (build-positional-error (numT) 1 l-type))]))))

; error messages
(define (build-error-message [strings : (listof string)]) : string
  (if (empty? strings) ""
      (string-append (first strings) (build-error-message (rest strings)))))

(define (build-positional-error-s [expected-type : string]
                                  [n : number]
                                  [received-type : Type]) : string
  (build-error-message 
   (list
    "type-error expected: " expected-type
    " in position " (to-string n)
    ", but found: " (to-string received-type))))

(define (build-positional-error [expected-type : Type]
                                [n : number]
                                [received-type : Type]) : string
  (build-positional-error-s (to-string expected-type) n received-type))

(define (build-if-error-message [t-type : Type][f-type : Type])
  (string-append
   "type-error expected same types in 2nd and 3rd positions but got "
  (to-string (list t-type f-type))))

(define (build-app-mismatch-error [expected-types : (listof Type)]
                                  [received-types : (listof Type)]) : string
  (build-error-message 
   (list
    "type-error expected: " (to-string expected-types) 
    " but found: " (to-string received-types))))

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
(test/exn (type-check-expr (eql (bool #t)(num 8))) 
          "eql: type-error expected: (numT) in position 1, but found: (boolT)")

; test ifthenelse
;  [ifthenelse (tst : TFAE) (thn : TFAE) (els : TFAE)]
(test (type-check-expr (ifthenelse (bool #t) (num 8)(num 9))) (numT))
(test (type-check-expr (ifthenelse (bool #f) (num 8)(num 9))) (numT))
(test/exn 
 (type-check-expr (ifthenelse (num 7) (num 8)(num 9))) 
 "if: type-error expected: (boolT) in position 1, but found: (numT)")
(test/exn 
 (type-check-expr (ifthenelse (bool #t) (num 8)(bool #f)))
 "type-error expected same types in 2nd and 3rd positions but got ((numT) (boolT))")

; test math
;  [add (l : TFAE) (r : TFAE)]
;  [sub (l : TFAE) (r : TFAE)]
(test (type-check-expr (add (num 7) (num 7))) (numT))
(test (type-check-expr (sub (num 7) (num 7))) (numT))
(test/exn (type-check-expr (add (bool #t) (num 7))) 
          "add: type-error expected: (numT) in position 1, but found: (boolT)")
(test/exn (type-check-expr (sub (bool #t) (num 7))) 
          "sub: type-error expected: (numT) in position 1, but found: (boolT)")
(test/exn (type-check-expr (add (num 7) (bool #t))) 
          "add: type-error expected: (numT) in position 2, but found: (boolT)")
(test/exn (type-check-expr (sub (num 7) (bool #t))) 
          "sub: type-error expected: (numT) in position 2, but found: (boolT)")

; test functions
;  [fun (args : (listof symbol)) (ts : (listof Type)) (body : TFAE)]
(test (type-check-expr 
       (fun (list 'x) (list (numT)) (add (num 5)(num 5)))) 
      (arrowT (list (numT)) (numT)))

(test (type-check-expr 
       (fun (list 'x) (list (numT)) (add (id 'x)(num 5)))) 
      (arrowT (list (numT)) (numT)))

(test (type-check-expr 
       (fun (list 'x 'y 'z) (list (numT)(numT)(numT)) (add (id 'x)(num 5)))) 
      (arrowT (list (numT)(numT)(numT)) (numT)))

(test (type-check-expr 
       (fun (list 'x 'y 'z) (list (numT)(numT)(numT)) (eql (id 'z) (add (id 'x)(id 'y))))) 
      (arrowT (list (numT)(numT)(numT)) (boolT)))

; function returning a function
(test (type-check-expr 
       (fun (list 'x) (list (numT)) (fun (list 'x) (list (boolT)) (num 5))))
      (arrowT (list (numT)) (arrowT (list (boolT)) (numT))))

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

; applying the function returning a function
; function returning a function
(test (type-check-expr 
       (app (fun (list 'x) (list (numT)) (fun (list 'x) (list (boolT)) (num 5))) (list (num 7))))
      (arrowT (list (boolT)) (numT)))

; same thing, applying twice
(test (type-check-expr 
       (app
        (app 
         (fun (list 'x) (list (numT)) (fun (list 'x) (list (boolT)) (num 5))) 
         (list (num 7)))
        (list (bool #t))))
      (numT))

(test/exn 
 (type-check-expr (app (num 8) (list (num 8))))       
 "app: type-error expected: function in position 1, but found: (numT)")

(test/exn 
 (type-check-expr
  ; pass a bool to a function wanting a num
  (app (fun (list 'x) (list (numT)) (add (id 'x)(num 5))) (list (bool #t))))
 "app: type-error expected: ((numT)) but found: ((boolT))")

(test/exn 
 (type-check-expr
  (app (fun (list 'x 'y) (list (numT) (boolT)) (id 'x)) (list (bool #t))))
 "app: type-error expected: ((numT) (boolT)) but found: ((boolT))")

(test/exn 
 (type-check-expr
  (app (fun (list 'x 'y) (list (numT) (boolT)) (id 'x)) (list (num 7) (num 7))))
 "app: type-error expected: ((numT) (boolT)) but found: ((numT) (numT))")

(test/exn 
 (type-check-expr
  (app (fun (list 'x 'y) (list (numT) (boolT)) (id 'x)) (list (bool #t) (bool #t))))
 "app: type-error expected: ((numT) (boolT)) but found: ((boolT) (boolT))")

; other tests
(test/exn 
 (type-check-expr (ifthenelse (eql (bool #t)(num 8)) (num 8)(num 9))) 
 "eql: type-error expected: (numT) in position 1, but found: (boolT)")