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

; the values yielded by the interpreter. 
(define-type RCFAE-Val
  [numV (n number?)]
  [closureV (param symbol?)
            (body RCFAE?)
            (env Env?)]
  [recV (fields (listof Id->Addr?))])

; an Env is a list of SymValPairs
(define-type SymValPair [symValPair (id symbol?)(val RCFAE-Val?)])
(define Env? (listof SymValPair?))

; a rec is basically a list of SymExprPair
(define-type SymExprPair [symExprPair (id symbol?)(expr RCFAE?)])
; a recV is a list of pointers, or Id->Addrs
(define-type Id->Addr [id->addr (id symbol?)(addr number?)])

; a MemLocation is simple, a number and a value (0 -> (numV 42))
(define-type MemLocation [memLoc (addr number?)(val RCFAE-Val?)])
; and a Mem is just a list of those (0 -> (numV 42), 1 -> (numV 54), ...)
(define Mem? (listof MemLocation?))

; interp yields a value and a (potentially updated) memory.
; Val-X-Mem is just a holder for those two values
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

; ive chosen to use the app and fun from hw5. 
; they support everything required for hw6, but
; they are more powerful and i felt no need to abandon them.
;; parse-app : sexpr -> add
;; {FAE1 FAE2 FAE3 FAE4} => {app {app {app 1 2} 3} 4}
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
  
;{fun {a b} {+ a b}} => {fun {a} {fun {b} {+ a b}}}
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

; arguably, i could pull most of these guys out into helper functions
; but ive only done so with interp-rec. still battling internally...
(define (interp expr env mem)
  ;(printf "size: ~s\n" (size mem))
  ;(print-mem mem)
  (type-case RCFAE expr
    [num (n) (vxm (numV n) mem)]
    [add (l r) 
      ; interpreting the left expr could change memory
      (type-case Val-X-Mem (interp l env mem)
        [vxm (lv lm)
          ; so interp the right exp with the new mem
          (type-case Val-X-Mem (interp r env lm)
            ; finally, yield the result and the memory
            ; from intepreting the right expr, since its most recent.
            [vxm (rv rm) (vxm (addV lv rv) rm)])])]
    [sub (l r) 
      ; sub same as add
      ; consider abstracting...though they are only 4 lines each.
      (type-case Val-X-Mem (interp l env mem)
        [vxm (lv lm)
          (type-case Val-X-Mem (interp r env lm)
            [vxm (rv rm) (vxm (subV lv rv) rm)])])]
    [id (name)
        ; id lookups look into the env. rec lookups go to memory.
        (vxm (env-lookup name env) mem)]
    [if0 (x y z)
      ; first inter the test expression
      (type-case Val-X-Mem (interp x env mem)
        [vxm (testv testm)
          ; then interp the y or z sub-expressions with the new memory
          (if (= 0 (ifop testv))(interp y env testm)(interp z env testm))])]
    [fun (id body) (vxm (closureV id body env) mem)]
    [app (fun-expr arg-expr)
      ; first interp the fun-expr
      (type-case Val-X-Mem (interp fun-expr env mem)
        [vxm (fv fm)
          ; then interp the arg with the new memory
          (type-case Val-X-Mem (interp arg-expr env fm)
            [vxm (av am)
              (type-case RCFAE-Val fv
                [closureV (id body cl-env)
                  ; finally interp the body with an augmented env
                  ; and the most recent memory
                  (interp body (cons (symValPair id av) cl-env) am)]
                [else (error "application expected procedure")])])])]
    [rec (fields) 
      ; sufficiently complicated to do elsewhere.
      (interp-rec expr env mem)]
    [get (rec id) 
      ; rec is an expression that must be interpreted. this could change mem.
      (type-case Val-X-Mem (interp rec env mem)
        [vxm (rv m)
          (type-case RCFAE-Val rv
            ; rec lookups go to memory, id lookups look into the env.
            ; first, rec-lookup returns a pointer to memory.
            ; we then go to memory to fetch the value.
            ; yield that result, and the memory from interpreting the rec expr.
            [recV (fields) (vxm (mem-lookup (rec-lookup id rv) m) m)]
            [else (error "record operation expected record")])])]
    [set (rec id new-val)
      ; rec is an expression that must be interpreted. this could change mem.
      (type-case Val-X-Mem (interp rec env mem)
        [vxm (v1 m1)
          ; new-val is an expression that must be interpreted. 
          ; this could change mem.
          ; interp it with the newly update memory
          (type-case Val-X-Mem (interp new-val env m1)
            [vxm (v2 m2)
              (type-case RCFAE-Val v1
                ; finally (as long as we have a rec)
                [recV (fields)
                  ; get the pointer to memory but looking up the id in the rec (v1)
                  (let ([pointer (rec-lookup id v1)])
                    ; get the old value from memory
                    (vxm (mem-lookup pointer m2) 
                         ; overwrite the old value with the new value
                         (mem-overwrite m2 pointer v2)))]
                [else (error "record operation expected record")])])])]
    [seqn (a b) 
     ; do the first thing, potentially updating memory
     (type-case Val-X-Mem (interp a env mem)
        [vxm (v1 m1)
          ; do the second thing with the updated mem
          (type-case Val-X-Mem (interp b env m1)
            [vxm (v2 m2)
              ; yield the result of the second expr,
              ; along with the memory from interpreting it.
              (vxm v2 m2)])])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  interp-rec: rec Env Mem -> Val-X-Mem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; easily the most complicated function in the interpreter.
; i use foldl, which, for each expression in the record
; interprets that expession with the newly updated memory from 
; interpreting the last expression. 
; it also accumulates the values of interpreting each expression.
(define (interp-rec rec env mem)
  ; interp-rec-field: SymExprPair (list (listof Id->Addr) Mem) -> 
  ;                   (list (listof Id->Addr) Mem)
  ; this is an inner helper function used in folding
  ; it takes the SymExprPair containing the expr to be interpd
  ; interprets the expression to a value and an updated memory
  ; finds the next open memory location
  ; adds the id of the origin SymExprPair and that memory pointer
  ; to the resulting list (listof Id->Addr)
  ; (that list is used to build the recV
  ; and updates the memory by adding the interpreted value
  ; finally, yields the updated list of pointers, and the new memory.
  ; thats a mouthful, and it might be easier just to read the code :)
  (define (interp-rec-field rec-field acc-fields-and-acc-mem)
    (let* ([current-id (symExprPair-id rec-field)]
           ; the expression to be interpd
           [current-expr (symExprPair-expr rec-field)]
           ; the fields accumulated thus far
           [acc-fields (first acc-fields-and-acc-mem)]
           [acc-mem (second acc-fields-and-acc-mem)])
      (type-case Val-X-Mem (interp current-expr env acc-mem)
        [vxm (v m)
           (let ([next-loc (next-location m)])
             (list 
              ; use of append here keeps recV fields in the same order as the rec
              (append acc-fields (list (id->addr current-id next-loc)))
              ; use of append here keeps the memory in ascending order, 0->N
              (append m (list (memLoc next-loc v)))))]
      )))
  (let ([interpd-fields-and-final-mem 
    (foldl interp-rec-field 
           ; start with an empty list of pointers 
           ; (because no rec field has been interpreted yet)
           ; and the current memory
           (list empty mem) 
           ; its the fields from the original rec that we are interpreting. 
           (rec-fields rec))])
    (vxm
     ; the list of pointers is the first thing returned from the fold
     (recV (first interpd-fields-and-final-mem))
     ; the final memory is the last thing returned from the fold. 
     (second interpd-fields-and-final-mem))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  memory helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; next-location: Mem -> num
; returns 1 higher than the last address in the given mem
; or 0, if the mem is empty
(define (next-location mem)
  (if (= 0 (length mem)) 0 
      (+ 1 (last (map (λ (x) (memLoc-addr x)) mem)))))

; mem-overwrite: Mem num RCFAE-Val -> mem
; overwrites the old value at the pointer location in the given mem
; with the given value
; yields a new memory
(define (mem-overwrite mem pointer val)
  (map (λ (ml) 
         (if (eq? (memLoc-addr ml) pointer) 
             (memLoc pointer val)
             ml)) mem))

(define (print-mem mem)
  (if (empty? mem) 
      (printf "empty")
      (begin (printf "~s\n" (car mem)) (print-mem (cdr mem)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  interp helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; helper to hide the other interp function, hiding memory altogether.
; (at least from the point of view of a user of the language)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(test (parse-app '(x y z)) (app (app (id 'x)(id 'y)) (id 'z)))
(test (parse-app '(w x y z)) (app (app (app (id 'w)(id 'x)) (id 'y)) (id 'z)))
(test (parse-app '(0 Q (+ 3 6))) (app (app (num 0) (id 'Q)) (add (num 3) (num 6))))


;; parse fun tests
(test/exn (parse-fun '(fun () x)) "bad syntax")
(test (parse-fun '(fun (x) x)) (fun 'x (id 'x)))
(test (parse-fun '(fun (x y) x)) (fun 'x (fun 'y (id 'x))))
(test (parse-fun '(fun (x x) x)) (fun 'x (fun 'x (id 'x))))
(test (parse-fun '(fun (x y z) (+ (+ x y) z))) 
      (fun 'x (fun 'y (fun 'z (add (add (id 'x) (id 'y))(id 'z))))))


;; parse seqn
(test (parse '(seqn 5 6)) (seqn (num 5)(num 6)))

;; parse seqn
(test (parse '(set (rec (x 6)) x 7)) 
      (set (rec (list (symExprPair 'x (num 6)))) 'x (num 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (mem-lookup 0 (list (memLoc 0 (numV 99)))) (numV 99))

(test (mem-lookup 1 (list (memLoc 0 (numV 99))
                          (memLoc 1 (numV 100)) 
                          (memLoc 2 (numV 10))
                          (memLoc 3 (numV 25)))) (numV 100))

(test (mem-lookup 3 (list (memLoc 0 (numV 99))
                          (memLoc 1 (numV 100)) 
                          (memLoc 2 (numV 10))
                          (memLoc 3 (numV 25)))) (numV 25))

(test/exn (mem-lookup 0 (list)) "bad address 0")
(test/exn (mem-lookup 1 (list)) "bad address 1")
(test/exn (mem-lookup 1 (list (memLoc 0 (numV 99)))) "bad address 1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interp tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some basics
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
;; function and application tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (interp-expr (parse '(fun (x) (+ 5 x)))) 'procedure)

(test (interp (parse '(fun (x) (+ 5 x))) '() '()) 
      (vxm (closureV 'x (add (num 5) (id 'x)) '()) '()))

; simple app
(test (interp-expr (parse '((fun (x) (+ 5 x)) 7))) 12)

; inner function with same id as outer function
(test (interp-expr (parse '(((fun (x) (fun (x) x)) 5) 6))) 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; record tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; interpret a simple record in an empty environment and mem
(test (interp (parse '{rec {a 10}}) '() '()) 
      (vxm (recV (list (id->addr 'a 0))) (list (memLoc 0 (numV 10)))))

; simple record with more than one field
(test (interp (parse '{rec {a 10}{b 25}}) '() '()) 
      (vxm (recV (list (id->addr 'a 0)(id->addr 'b 1))) 
           (list (memLoc 0 (numV 10))(memLoc 1 (numV 25)))))

; simple record with more than one field, with existing mem
(test (interp (parse '{rec {a 10}{b 25}}) 
              '() 
              ; the mem before we interp the record
              (list (memLoc 0 (numV 99))(memLoc 1 (numV 100)))) 
      (vxm 
       ; notice the pointers are to 2 and 3, instead of 0 and 1 like last test.
       (recV (list (id->addr 'a 2)(id->addr 'b 3))) 
       ; the mem after
       (list (memLoc 0 (numV 99))
             (memLoc 1 (numV 100)) 
             (memLoc 2 (numV 10))
             (memLoc 3 (numV 25)))))

; rec with inner recs.
(test (interp (parse '{rec {a {rec {x 99}{y 100}}}{b 25}}) '() '()) 
      (vxm (recV (list (id->addr 'a 2)(id->addr 'b 3)))
          (list 
           ; the inner record gets interpd first
           ; and its x value goes into slot 0
           (memLoc 0 (numV 99))
           ; similarly, its y value is stored in slot 1
           (memLoc 1 (numV 100)) 
           ; the 'a value of the outer record is the inner record
           ; notice its pointers to 0 and 1
           (memLoc 2 (recV (list (id->addr 'x 0)(id->addr 'y 1))))
           ; b, boring last value
           (memLoc 3 (numV 25)))))


(test (interp-expr (parse 
  '{with {r {with {x 3}{rec {y x}}}} {with {x 4} {get r y}}})) 
  3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (interp-expr (parse '{get {rec {r 1}} r})) 1)
(test (interp-expr (parse '{get {rec {r {rec {z 0}}}} r})) 'rec)
(test (interp-expr (parse '{get {rec {a 10} {b {+ 1 2}}} b})) 3)
(test (interp-expr 
       (parse '{+ 10 {get {get {rec {r {rec {z 10}}}} r} z}})) 20)
; nested get
(test (interp-expr (parse '{get {get {rec {r {rec {z 0}}}} r} z})) 0)

(test (interp-expr (parse '{{fun {r}{get r x}}{rec {x 1}}})) 1)

(test/exn (interp-expr (parse `(get 1 x))) "record operation expected record") 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; seqn tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; this test doesnt prove that the first expression gets executed.
(test (interp-expr (parse '{seqn 7 8})) 8)

; this test does!
(test (interp (parse '{seqn {rec {a 10}} 8}) '() '()) 
      (vxm (numV 8) (list (memLoc 0 (numV 10)))))

; and this one is just for good measure
(test (interp (parse '{seqn 8 {rec {a 10}}}) '() '()) 
      (vxm (recV (list (id->addr 'a 0))) (list (memLoc 0 (numV 10)))))

; ahh why the heck not...(update memory in both expressions)
(test (interp (parse '{seqn {rec {a 10}} {rec {b 11}}}) '() '()) 
      (vxm (recV (list (id->addr 'b 1))) 
           (list (memLoc 0 (numV 10))(memLoc 1 (numV 11)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; set on a simple record yields old val and updated memory
(test (interp (parse '{set {rec {a 10}} a 42}) '() '()) 
      (vxm (numV 10) (list (memLoc 0 (numV 42)))))

(test (interp-expr (parse 
  '{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}})) 5)

(test (interp-expr (parse `{set {rec {x 42}} x 2})) 42)

(test (interp-expr (parse 
  '{{{{{fun {g}
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

(test/exn (interp-expr (parse '(set (rec) p (fun (p) y)))) "unknown field p")
(test/exn 
 (interp-expr (parse '(set (rec (p 4) (k 4)) q (with (q 1) 6)))) 
 "unknown field q")

(test/exn 
 (interp-expr (parse `(set 1 x (rec (x 2))))) "record operation expected record")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; this works. commented out because it will run forever.
;(test (interp-expr (parse 
;  '{with {b {rec {x 1}}} 
;         {with {f 
;                {fun {f} {seqn {set b x 2} {f f}}}
;                } {f f}}
;         })) (error "unreachable"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEYOND HOMEWORK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; everything below this point is robbed from my last homework.
; i figure that the language from this homework is a superset
; of the language from last homework, so the tests better all work. they do.

; some random thoughts though...
; i could probably add some more interesting libraries, but dont have time.
; however, i do wish we were asked to build some things with this language
; as part of the assignment. i guess all/most of us are used to using languages
; with mutation, so maybe its not that important, but might have been fun.
; but also, i can imagine that this homework was pretty challenging for some,
; and so adding more to it might have been overwhelming. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; primitive functions
(define pair `{fun {l r} {fun {b} {b l r}}})
(define fst  `{fun {p} {with {t {fun {x y} x}}{p t}}})
(define snd  `{fun {p} {with {f {fun {x y} y}}{p f}}})

; Y combinator
(define Y
  `(fun (X)
    ((fun (p) (X (fun (arg) ((p p) arg))))
     (fun (p) (X (fun (arg) ((p p) arg)))))))

(define addf `{fun {x y} {+ x y}})

(define (create-lib depends-on-libs pairs)
  (define (create-lib-func sym body env)
    (cons (symValPair sym (vxm-v (interp (parse body) env '()))) env))
  (foldl
   (lambda (pair acc-env)
     (create-lib-func (first pair) (second pair) acc-env))
   (foldl append '() depends-on-libs)
   pairs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define base-lib 
  (create-lib '()
   (list (list 'Y Y)
         (list 'pair pair)
         (list 'fst fst)
         (list 'snd snd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boolean library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define boolean-lib
  (create-lib '()
   (list
    (list 'or '(fun (x y) (if0 x 0 (if0 y 0 1))))
    (list 'and '(fun (x y) (if0 x (if0 y 0 1) 1)))
    (list 'zero? '(fun (x) (if0 x 0 1)))
    (list 'not '(fun (x) (if0 x 1 0)))
    )))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; math library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define math-lib
  (create-lib (list boolean-lib base-lib) 
   (list
    (list 'addf addf)
    (list 'split-neg? '(Y (fun (SN)(fun (p n) (if0 (or (zero? p)(zero? n)) 1
      (if0 (+ n 1) 0 (if0 (zero? (- p 1)) 1 (SN (- p 1) (+ n 1)))))))))
    (list 'neg? '(fun (x) (split-neg? x x)))
    (list 'pos? '(fun (x) (and (not (zero? x)) (not (neg? x)))))
    (list 'abs '(fun (x) (if0 (neg? x) (- 0 x) x)))
    (list 'add-n-times 
          '(Y (fun (NX) (fun (n x) (if0 n 0 (+ x (NX (- n 1) x)))))))
    (list 'mult '(fun (x y)
      (with (n (add-n-times (abs x) y)) (if0 (pos? x) n (- 0 n)))))
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list-lib
  (create-lib (list math-lib base-lib)
   (list 
    (list 'mapreduce  
          `{Y {fun {MR} 
                   {fun {f g id xs} 
                        {if0 xs id {g {f {fst xs}} {MR f g id {snd xs}}}}}}})
    (list 'reduce `{fun {g id xs} {mapreduce {fun {x} x} g id xs}})
    (list 'map `{fun {f xs} {mapreduce f {fun {x y} {pair x y}} 0 xs}})
    (list 'sum `{fun {l} {reduce addf 0 l}}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; finally, the entire fae library.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fae-lib (create-lib (list list-lib math-lib boolean-lib base-lib) '()))
;(define fae-lib (create-lib (list math-lib boolean-lib base-lib) '()))

;; parse the expression, and then interpret (with access to the main library)
(define (fae sexpr)
  (type-case RCFAE-Val (vxm-v (interp (parse sexpr) fae-lib '()))
             [numV (n) n]
             [closureV (s b ds) 'procedure]
             [recV (fields) 'rec]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smoke test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (fae '(+ 5 6)) 11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pair tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (fae '(fst (pair 2 3))) 2)
(test (fae '(snd (pair 2 0))) 0)
(test (fae '{snd {pair 1 2}}) 2)
(test (fae '{snd {fst {pair {pair 1 2} {pair 3 4}}}}) 2)
(test (fae '{with {p {pair 1 2}}{+ {fst p} {snd p}}}) 3)
(test (fae '{with {p {pair {fun {x} {+ x 1}} 2}}{{fst p} {snd p}}}) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list library tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; sum
(test (fae '(sum 0)) 0)
(test (fae '(sum (pair 1 0))) 1)
(test (fae '(sum (pair 2 (pair 1 0)))) 3)
(test (fae '(sum (pair 9 (pair 2 (pair 1 0))))) 12)
(test (fae '(sum (pair 0 (pair 9 (pair 2 (pair 1 0)))))) 12)
(test (fae '(sum (pair 0 (pair 0 (pair 0 (pair 0 0)))))) 0)

; reduce

(test (fae `(reduce addf 0 (pair 0 (pair 0 (pair 0 (pair 0 0)))))) 0)
(test (fae 
       `(reduce addf 5 (pair 10 (pair 20 (pair 30 (pair 40 0)))))) 105)

; map

; since lists are functions, i cant easily test the results of map for equality
; so instead, i map, then sum, then check the answer. 

(test (fae 
       `(sum
         (map (fun (x) (+ x 10)) (pair 0 (pair 0 (pair 0 (pair 0 0)))))))
      40)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; math lib tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (fae '(neg? 5)) 1)
(test (fae '(neg? 1)) 1)
(test (fae '(neg? 0)) 1)
(test (fae '(neg? -1)) 0)
(test (fae '(neg? -5)) 0)
 
(test (fae '(add-n-times 3 3)) 9)
(test (fae '(add-n-times 3 7)) 21)
(test (fae '(add-n-times 50 10)) 500)
 
(test (fae '(mult 3 3)) 9)
(test (fae '(mult -3 -3)) 9)
(test (fae '(mult -3 3)) -9)
(test (fae '(mult 3 -3)) -9)
(test (fae '(mult 0 0)) 0)
(test (fae '(mult 9 0)) 0)
(test (fae '(mult 0 9)) 0)
(test (fae '(mult -9 0)) 0)
(test (fae '(mult 0 -9)) 0)
(test (fae '(mult 100 100)) 10000)
 
(test (fae '(pos? 0)) 1)
(test (fae '(pos? 1)) 0)
(test (fae '(pos? -1)) 1)
(test (fae '(pos? 2)) 0)
(test (fae '(pos? -2)) 1)
 
(test (fae '(and 1 1)) 1)
(test (fae '(and 0 1)) 1)
(test (fae '(and 1 0)) 1)
(test (fae '(and 0 0)) 0)

(test (fae '(or 1 1)) 1)
(test (fae '(or 0 1)) 0)
(test (fae '(or 1 0)) 0)
(test (fae '(or 0 0)) 0)
 
(test (fae '(zero? 1)) 1)
(test (fae '(zero? 0)) 0)
(test (fae '(zero? -1)) 1)
 
(test (fae '(not 1)) 0)
(test (fae '(not 0)) 1)
 
(test (fae '(abs -1)) 1)
(test (fae '(abs 1)) 1)
(test (fae '(abs 0)) 0)
(test (fae '(abs -5)) 5)
(test (fae '(abs 5)) 5)
