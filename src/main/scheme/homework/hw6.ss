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
  [seqn (a RCFAE?)(b RCFAE?)]
  [set (r RCFAE?)(id symbol?)(val RCFAE?)])

(define-type RCFAE-Val
  [numV (n number?)]
  [closureV (param symbol?)
            (body RCFAE?)
            (env Env?)]
  [recV (fields (listof Id->Addr?))])

(define-type SymExprPair [symExprPair (id symbol?)(expr RCFAE?)])
(define-type SymValPair [symValPair (id symbol?)(val RCFAE-Val?)])
(define-type Id->Addr [id->addr (id symbol?)(addr number?)])
(define-type MemLocation [memLoc (addr number?)(val RCFAE-Val?)])
(define Env? (listof SymValPair?))
(define Mem? (listof MemLocation?))
(define-type Val-X-Mem [vxm (v RCFAE-Val?)(m Mem?)])

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
       [else (parse-app sexpr)])]
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
  
;; {fun {a b} {+ a b}} => {fun {a} {fun {b} {+ a b}}}
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
;; interp : RCFAE evn mem -> RCFAE-Val-X-Mem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interp expr env mem)
  ;(printf "size: ~s\n" (size mem))
  ;(print-mem mem)
  (type-case RCFAE expr
    [num (n) (vxm (numV n) mem)]
    [add (l r) 
      (type-case Val-X-Mem (interp l env mem)
        [vxm (lv lm)
          (type-case Val-X-Mem (interp r env lm)
            [vxm (rv rm) (vxm (addV lv rv) rm)])])]
    [sub (l r) 
      (type-case Val-X-Mem (interp l env mem)
        [vxm (lv lm)
          (type-case Val-X-Mem (interp r env lm)
            [vxm (rv rm) (vxm (subV lv rv) rm)])])]
    [id (name) (vxm (env-lookup name env) mem)]
    [if0 (x y z)
      (type-case Val-X-Mem (interp x env mem)
        [vxm (testv testm)
          (if (= 0 (ifop testv))(interp y env testm)(interp z env testm))])]
    [fun (id body) (vxm (closureV id body env) mem)]
    [app (fun-expr arg-expr)
      (type-case Val-X-Mem (interp fun-expr env mem)
        [vxm (fv fm)
          (type-case Val-X-Mem (interp arg-expr env fm)
            [vxm (av am)
              (type-case RCFAE-Val fv
                [closureV (id body cl-env)
                  (interp body (cons (symValPair id av) cl-env) am)]
                [else (error "application expected procedure")])])])]
    [rec (fields) (interp-rec expr env mem)]
    [get (rec id) 
      (type-case Val-X-Mem (interp rec env mem)
        [vxm (rv m)
          (type-case RCFAE-Val rv
            [recV (fields) (vxm (mem-lookup (rec-lookup id rv) m) m)]
            [else (error "record operation expected record")])])]
    [set (rec id new-val)
      (type-case Val-X-Mem (interp rec env mem)
        [vxm (v1 m1)
          (type-case Val-X-Mem (interp new-val env m1)
            [vxm (v2 m2)
              (type-case RCFAE-Val v1
                [recV (fields)
                  (let* ([pointer (rec-lookup id v1)]
                         [old-val (mem-lookup pointer m2)])
                    (vxm old-val (mem-overwrite m2 pointer v2)))]
                [else (error "record operation expected record")])])])]
    [seqn (a b) 
     (type-case Val-X-Mem (interp a env mem)
        [vxm (v1 m1)
          (type-case Val-X-Mem (interp b env m1)
            [vxm (v2 m2)
              (vxm v2 m2)])])]))

(define (next-location mem)
  (if (= 0 (length mem)) 0 
      (+ 1 (last (sort (map (λ (x) (memLoc-addr x)) mem) <)))))
  
(define (mem-overwrite mem pointer val)
  (map (λ (ml) (if (eq? (memLoc-addr ml) pointer) 
                   (memLoc pointer val)
                   ml)) mem))

(define (print-mem mem)
  (if (empty? mem) 
      (printf "empty")
      (begin (printf "~s\n" (car mem)) (print-mem (cdr mem)))))

; interp-rec: rec env mem -> vxm
(define (interp-rec rec env mem)
  (let ([interpd-fields-and-final-mem 
    (foldl 
      (lambda (rec-field acc-fields-and-acc-mem)
        (let* ([current-id (symExprPair-id rec-field)]
               [current-expr (symExprPair-expr rec-field)]
               [acc-fields (first acc-fields-and-acc-mem)] 
               [acc-mem (second acc-fields-and-acc-mem)])
          (type-case Val-X-Mem (interp current-expr env acc-mem)
            [vxm (v m)   
              (let ([next-loc (next-location m)])
                (list (append acc-fields (list (id->addr current-id next-loc)))
                      (append m (list (memLoc next-loc v)))))]
                 ))) (list empty mem) (rec-fields rec))])
    (vxm (recV (first interpd-fields-and-final-mem))
         (second interpd-fields-and-final-mem))))

;Provide a definition of interp-expr : RCFAE -> number or 'procedure, as above.
(define (interp-expr expr) 
  (type-case Val-X-Mem (interp expr empty empty)
    [vxm (v a)
      (type-case RCFAE-Val v
        [numV (n) n]
        [closureV (s b ds) 'procedure]
        [recV (fields) 'rec])]))

(define (addV l r) (mathV + l r))
(define (subV l r) (mathV - l r))
(define (mathV op l r)
  (if 
   (and (numV? l) (numV? r)) 
   (numV (op (numV-n l)(numV-n r)))
   (error "numeric operation expected number")))
(define (ifop n) (if (numV? n) (numV-n n) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; env-lookup: name env -> RCFAE-Val
(define (env-lookup name env)
  (symValPair-val (find-or-die 
    (λ (f) (symbol=? (symValPair-id f) name)) env
    (string-append "free identifier: " (symbol->string name)))))

; mem-lookup: num mem -> RCFAE-Val
(define (mem-lookup addr memory)
  (memLoc-val (find-or-die 
    (λ (f) (eq? (memLoc-addr f) addr)) memory
    (string-append "bad address " (number->string addr)))))

;; rec-lookup : recV sym -> num
(define (rec-lookup id rec)
  (id->addr-addr (find-or-die 
    (λ (f) (symbol=? (id->addr-id f) id)) (recV-fields rec)
    (string-append "unknown field " (symbol->string id)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Option [none] [some (v (λ (x) #t))])
  
(define (findo p l)
  (cond [(empty? l) (none)]
        [(p (first l)) (some (first l))]
        [else (findo p (cdr l))]))

;; find-or-die: predicate list[T] string -> T
(define (find-or-die p l error-string)
  (type-case Option (findo p l) [some (v) v] [none () (error error-string)]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse basics
(test (parse 7) (num 7))
(test (parse 'a) (id 'a))
(test (parse '(+ 6 7)) (add (num 6) (num 7)))
(test (parse '(- 6 7)) (sub (num 6) (num 7)))
(test (parse '(+ 6 (+ 6 7))) (add (num 6) (add (num 6) (num 7))))
(test (parse '(- 6 (- 6 7))) (sub (num 6) (sub (num 6) (num 7))))
(test (parse '(with (x 7) x)) (app (fun 'x (id 'x)) (num 7)))
(test (parse '(if0 0 1 2)) (if0 (num 0) (num 1) (num 2)))

;; recs
(test (parse '(rec (x 6))) (rec (list (symExprPair 'x (num 6)))))
(test (parse '(rec (x (+ 6 7)))) 
      (rec (list (symExprPair 'x (add (num 6)(num 7))))))
(test (parse '(get (rec (x 7)) x)) 
      (get (rec (list (symExprPair 'x (num 7)))) 'x))
(test (parse '(get z x)) (get (id 'z) 'x))

;; parser allows (+ rec rec), must fail in interpreter
(test (parse '(+ (rec (x 6)(y 7)) (rec (x 6)(y 7)))) 
      (add (rec (list (symExprPair 'x (num 6))(symExprPair 'y (num 7))))
           (rec (list (symExprPair 'x (num 6))(symExprPair 'y (num 7))))))

; parser error case
(test/exn (parse-rec '{rec {a 0} {a 12}}) "duplicate fields")

;; parse application tests
(test/exn (parse-app '(x)) "appliction without arguments")
(test (parse-app '(x y)) (app (id 'x)(id 'y)))

;; parse fun tests
(test/exn (parse-fun '(fun () x)) "bad syntax")
(test (parse-fun '(fun (x) x)) (fun 'x (id 'x)))
(test (parse '((fun (x) (+ x 2)) 5)) 
      (app (fun 'x (add (id 'x) (num 2))) (num 5)))

;; parse seqn
(test (parse '(seqn 5 6)) (seqn (num 5)(num 6)))

;; parse seqn
(test (parse '(set (rec (x 6)) x 7)) 
      (set (rec (list (symExprPair 'x (num 6)))) 'x (num 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interp-expr tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tests not requiring the library functions, just the interpreter
(test (interp-expr (parse 0)) 0)
(test (interp-expr (parse (- 10 3))) 7)
(test (interp-expr (parse (+ 3 4))) 7)
(test (interp-expr (parse '((fun (x) (+ x 2)) 5))) 7)
(test (interp-expr (parse '(((fun (x) (fun (y) (+ x y))) 5 )2))) 7)
(test (interp-expr (parse '((fun (x) (fun (y) (+ x y))) 5))) 'procedure)
(test (interp-expr (parse '(fun (x) (+ x 2)))) 'procedure)
(test (interp-expr (parse '(if0 0 1 2))) 1)
(test (interp-expr (parse '(if0 1 1 2))) 2)

;; bad math test
(test/exn (interp-expr (parse '{+ {fun {x} x} 1})) 
          "numeric operation expected number")

;; bad application test
(test/exn (interp-expr (parse '{1 2})) "application expected procedure")
(test/exn (interp-expr (parse '((0 Q) (+ 3 6)))) "free identifier")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (interp-expr (parse '(((fun (x) (fun (x) x)) 5) 6))) 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; record tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; interpret a simple record in an empty environment and mem
(test (interp (parse '{rec {a 10}}) '() '()) 
      (vxm (recV (list (id->addr 'a 0))) (list (memLoc 0 (numV 10)))))

;; interpret a simple record in an non empty environment and mem
;(test (interp 
;       (parse '{rec {a {- x y}}}) 
;        ; the env
;       (list (id->addr 'x 0) (id->addr 'y 1))
;        ; the mem
;       (list (memLoc 0 (numV 77))(memLoc 1 (numV 44))))
;      (vxm 
;       ; the record
;       (recV (list (id->addr 'a 2))) 
;       ; the update memory
;       (list (memLoc 0 (numV 77))(memLoc 1 (numV 44))(memLoc 2 (numV 33)))))
;
; simple record with more than one field
(test (interp (parse '{rec {a 10}{b 25}}) '() '()) 
      (vxm (recV (list (id->addr 'a 0)(id->addr 'b 1))) 
           (list (memLoc 0 (numV 10))(memLoc 1 (numV 25)))))

; get
(test (interp-expr (parse '{get {rec {r 1}} r})) 1)
(test (interp-expr (parse '{get {rec {r {rec {z 0}}}} r})) 'rec)
(test (interp-expr (parse '{get {rec {a 10} {b {+ 1 2}}} b})) 3)
(test (interp-expr 
       (parse '{+ 10 {get {get {rec {r {rec {z 10}}}} r} z}})) 20)
; nested get
(test (interp-expr (parse '{get {get {rec {r {rec {z 0}}}} r} z})) 0)

; seqn
(test (interp-expr (parse '{seqn 7 8})) 8)

; set 
(test (interp-expr (parse '{{fun {r}{get r x}}{rec {x 1}}})) 1)

(test (interp-expr (parse 
  '{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}})) 5)

(test (interp-expr (parse `{set {rec {x 42}} x 2})) 42)

(test (interp-expr (parse '{{{{{fun {g}
                                    {fun {s}
                                         {fun {r1}
                                              {fun {r2}
                                                   {+ {get r1 b}
                                                      {seqn
                                                       {{s r1} {g r2}}
                                                       {+ {seqn
                                                           {{s r2} {g r1}}
                                                           {get r1 b}}
                                                          {get r2 b}}}}}}}}
                               {fun {r} {get r a}}}            ; g
                              {fun {r} {fun {v} {set r b v}}}} ; s
                             {rec {a 0} {b 2}}}                ; r1
                            {rec {a 3} {b 4}}}))               ; r2
      5)


; tests that failed (some of them were just error message differences, tho...)
(test/exn (interp-expr (parse '(set (rec) p (fun (p) y)))) "unknown field p")
(test/exn (interp-expr (parse '(set (rec (p 4) (k 4)) q (with (q 1) 6)))) "unknown field q")
(test/exn (interp-expr (parse `(get 1 x))) "record operation expected record") 
(test/exn (interp-expr (parse `(set 1 x (rec (x 2))))) "record operation expected record") 


;; part two
;(test (interp-expr (parse 
;  '{with {b {rec {x 1}}} 
;         {with {f 
;                {fun {f} {seqn {set b x 2} {f f}}}
;                } {f f}}
;         })) (error "unreachable"))
