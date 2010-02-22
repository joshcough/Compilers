#lang planet plai/plai:1:19/collector
(print-only-errors #t)

;; init-allocator : -> void
(define (init-allocator)
  ; 16 at least allows us to alloc two flats.
  (when (< (heap-size) 16) (error 'too-small))
  ; the pointer to the currently active semi-space is at position 0
  ; this will toggle between 4 (start of first semi-space) and
  ; the start of the second semispace
  (set-current-semispace (start-of-first-semispace))
  ; reserve these guys for now til i figure out what to do with them
  (heap-set! 2 'reserved)
  (heap-set! 3 'reserved)
  (put-constants-in-heap)
  (for ([i (in-range reserved-spaces-at-start-of-heap (heap-size))])
    (heap-set! i 'free))
  (unless (even? (heap-size)) (heap-set! (- (heap-size) 1) 'wasted)))

(define (put-constants-in-heap)
  (heap-set! 4 '())
  (heap-set! 5 #f)
  (heap-set! 6 #t)
  ; put constants [0-4] in the heap
  (for ([i (in-range 0 5)]) 
    ; 7 for the 4 pointers, and then () #f #t
    (heap-set! (+ i 7) i)))

(define reserved-spaces-at-start-of-heap 12)
(define (start-of-first-semispace) reserved-spaces-at-start-of-heap)

(define (semi-space-size) 
  (/ (- (heap-size) reserved-spaces-at-start-of-heap) 2))

;; start-of-second-semispace : -> number
;; the address of 2nd half of the heap
(define (start-of-second-semispace) 
  (+ (start-of-first-semispace) (semi-space-size)))

(define (end-of-first-semispace) (- (start-of-second-semispace) 1))
(define (end-of-second-semispace) 
  (if (even? (heap-size)) 
      (- (heap-size) 1)
      (- (heap-size) 2)))
(define (end-of-current-semispace)
  (if (eq? (get-current-semispace) (start-of-first-semispace)) 
      (end-of-first-semispace)
      (end-of-second-semispace)))

(define (switch-semispace)
  (set-current-semispace 
   (if (eq? (get-current-semispace) (start-of-first-semispace)) 
       (start-of-second-semispace)
       (start-of-first-semispace))))

(define (get-current-semispace) (heap-ref 0))
(define (set-current-semispace loc) 
  (heap-set! 0 loc)
  (set-allocation-pointer loc))

(define (get-other-semispace) 
  (if (eq? (get-current-semispace) (start-of-first-semispace)) 
       (start-of-second-semispace)
       (start-of-first-semispace)))

(define (get-allocation-pointer) (heap-ref 1))
(define (set-allocation-pointer loc) 
  (heap-set! 1 loc))

;; gc:deref : loc -> heap-value
;; must signal an error if fl-loc doesn't point to a flat value
(define (gc:deref fl-loc) 
  ;(printf "deref : ~s\n" fl-loc) 
  (cond
    [(imm-loc? fl-loc) (heap-ref fl-loc)]
    [else (unless (gc:flat? fl-loc)
            (error 'gc:deref "not flat"))
          (heap-ref (+ 1 fl-loc))]))

(define (space-available? n)
  (<= (+ n (get-allocation-pointer))(+ 1 (end-of-current-semispace))))

;; gc:alloc-flat : heap-value -> loc
(define (gc:alloc-flat fv)
  (cond
    [(imm-val? fv) (imm-value->loc fv)]
    [else
     (if (space-available? 2)
         (really-alloc-flat fv)
         (begin
           (collect-garbage (if (procedure? fv)
                                (map read-root (procedure-roots fv))
                                '()))
           (if (space-available? 2)
               (really-alloc-flat fv)
               (error "out of memory"))))]))

(define (really-alloc-flat fv)
  ;(printf "really-alloc-flat: ~s allocation pointer ~s:\n" fv (get-allocation-pointer))
  (let ([n (get-allocation-pointer)])
        (heap-set! n 'flat)
        (heap-set! (+ 1 n) fv)
        (set-allocation-pointer (+ 2 n))
        n))

(define (imm-loc? loc) 
  (and 
   (> loc 3)
   (< loc reserved-spaces-at-start-of-heap)))

(define (imm-loc->value loc)
  (case loc
    [(4) '()]
    [(5) #f]
    [(6) #t]
    [else (- loc 7)]))

(define (imm-val? fv)
  (or (and (exact-integer? fv)
           (<= 0 fv 4))
      ; TODO - convert to cond
      (and (member fv (list '() #t #f)) #t)))

(define (imm-value->loc fv)
  (case fv
  [(()) 4]
  [(#f) 5]
  [(#t) 6]
  [else (+ fv 7)]))

;; gc:cons : loc loc -> loc
;; hd and tl are guaranteed to have been earlier
;; results from either gc:alloc-flat or gc:cons
(define (gc:cons hd tl) 
    (if (space-available? 3)
        (really-alloc-cons hd tl)
        (begin
        (collect-garbage (list hd tl))
        (if (space-available? 3)
            ;hd and tl will now be at the front of the new semispace
            (really-alloc-cons 
             (get-current-semispace) 
             (+ (get-current-semispace) (if (gc:flat? (get-current-semispace)) 2 3)))
            (error "out of memory")))))

(define (really-alloc-cons hd tl)
  (let ([n (get-allocation-pointer)])
        (heap-set! n 'pair)
        (heap-set! (+ 1 n) hd)
        (heap-set! (+ 2 n) tl)
        (set-allocation-pointer (+ 3 n))
        n))

;; gc:first : loc -> loc
;; must signal an error of pr-loc does not point to a pair
(define (gc:first pr-loc) 
  (unless (gc:cons? pr-loc)
    (error 'gc:first "not a pair"))
  (heap-ref (+ 1 pr-loc)))

;; gc:rest : loc -> loc
;; must signal an error of pr-loc does not point to a pair
(define (gc:rest pr-loc)
  (unless (gc:cons? pr-loc)
    (error 'gc:first "not a pair"))
  (heap-ref (+ 2 pr-loc)))

;; gc:flat? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:flat? loc)
  (cond
    [(imm-loc? loc) #t]
    [else
     (equal? (heap-ref loc) 'flat)]))

;; gc:cons? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:cons? loc)
  (cond
    [(imm-loc? loc) #f]
    [else
     (equal? (heap-ref loc) 'pair)]))

;; gc:set-first! : loc loc -> void
;; must signal an error of pr-loc does not point to a pair
(define (gc:set-first! pr-loc new) 
  (unless (gc:cons? pr-loc)
    (error 'gc:set-first! "not a pair"))
  (heap-set! (+ 1 pr-loc) new))

;; gc:set-rest! : loc loc -> void
;; must signal an error of pr-loc does not point to a pair
(define (gc:set-rest! pr-loc new)   
  (unless (gc:cons? pr-loc)
    (error 'gc:set-first! "not a pair"))
  (heap-set! (+ 2 pr-loc) new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gc                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (collect-garbage extra-roots) 
  (begin
    ;(printf "collecting garbage: ~s ~s\n" (map read-root (get-root-set)) extra-roots) 
    ;(printf "current semispace: ~s\n" (get-current-semispace))
    (switch-semispace)
    ;(printf "current semispace after switch: ~s\n" (get-current-semispace))
    ; change these lambdas to a top level proc
    (for-each move-root extra-roots)
    (for-each move-root (get-root-set))
    (cleanup-semispace (get-other-semispace))))

(define (cleanup-semispace loc)
  (for ([i (in-range loc (+ loc (semi-space-size)))])(heap-set! i 'freed)))

; if here just so i can do testing using things
; that arent really plai roots
(define (move-root root)
  ;(printf "move root: ~s\n" (if (root? root)(read-root root) root)) 
  (if (root? root)
      (let ([new-loc (move-obj (read-root root))]) 
        ;(printf "moving root to: ~s\n" new-loc)
        (set-root! root new-loc)
        new-loc)
      (move-obj root)))

(define (move-proc-root root)
  (unless (in-current-semispace (read-root root))
    (move-root root)))
 
(define (in-current-semispace loc)
  (if (eq? (get-current-semispace) (start-of-first-semispace)) 
      (< loc (start-of-second-semispace))
      (>= loc (start-of-second-semispace))))

(define (move-obj loc)
  ;(printf "move obj: ~s\n" loc) 
  (cond
    [(imm-loc? loc) loc]
    [(gc:flat? loc) (move-flat loc)]
    [(gc:cons? loc) (move-cons loc)]
    [(eq? 'flat-forward (heap-ref loc)) (heap-ref (+ 1 loc))]
    [(eq? 'cons-forward (heap-ref loc)) (heap-ref (+ 1 loc))]
    [else (begin (printf "loc: ~s\n" loc)
                 (printf "heap-ref loc: ~s\n" (heap-ref loc)) 
                 (error "how did we get here?"))]))

(define (move-flat loc)
  ;(printf "moving flat: ~s ~s\n" loc (heap-ref (+ 1 loc))) 
  (let ([v (heap-ref (+ 1 loc))])
    (if (procedure? v) 
        (move-proc loc v)
        (let ([new-loc (really-alloc-flat (gc:deref loc))])
          ;(printf "new-loc: ~s\n" new-loc) 
          (heap-set! loc 'flat-forward)
          (heap-set! (+ 1 loc) new-loc) 
          new-loc))))

(define (move-proc loc v) 
  ;(printf "move-proc! ~s\n" (map read-root (procedure-roots v)))
  (let ([new-loc (really-alloc-flat (gc:deref loc))])
    (heap-set! loc 'flat-forward)
    (heap-set! (+ 1 loc) new-loc)
    (for-each move-proc-root (procedure-roots v))
  new-loc))

(define (move-cons loc)
  ;(printf "move cons: ~s\n" loc) 
  (heap-set! loc 'cons-forward)
  (let ([new-loc (really-alloc-cons (heap-ref (+ 1 loc))(heap-ref (+ 2 loc)))])
    (heap-set! loc 'cons-forward)
    (heap-set! (+ 1 loc) new-loc)
    (heap-set! (+ 2 loc) 'freed-by-gc)
    (let ([x (move-obj (heap-ref (+ 1 new-loc)))])
      (heap-set! (+ 1 new-loc) x))
    (let ([x (move-obj (heap-ref (+ 2 new-loc)))])
      (heap-set! (+ 2 new-loc) x))
    new-loc ))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests                                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (with-heap (make-vector 16 #f) (end-of-first-semispace)) 13)
(test (with-heap (make-vector 16 #f) (end-of-second-semispace)) 15)

(test (with-heap (make-vector 18 #f) (end-of-first-semispace)) 14)
(test (with-heap (make-vector 18 #f) (start-of-second-semispace)) 15)
(test (with-heap (make-vector 18 #f) (end-of-second-semispace)) 17)

(test (with-heap (make-vector 64 #f) (start-of-first-semispace)) 12)
(test (with-heap (make-vector 64 #f) (end-of-first-semispace)) 37)
(test (with-heap (make-vector 64 #f) (start-of-second-semispace)) 38)
(test (with-heap (make-vector 64 #f) (end-of-second-semispace)) 63)

(test (with-heap (make-vector 100 #f) (start-of-first-semispace)) 12)
(test (with-heap (make-vector 100 #f) (end-of-first-semispace)) 55)
(test (with-heap (make-vector 100 #f) (start-of-second-semispace)) 56)
(test (with-heap (make-vector 100 #f) (end-of-second-semispace)) 99)

(test (imm-loc? 3) #f)
(test (imm-loc? 4) #t)
(test (imm-loc? 11) #t)
(test (imm-loc? 12) #f)

(test (imm-val? '()) #t)
(test (imm-val? #f) #t)
(test (imm-val? #t) #t)
(test (imm-val? 0) #t)
(test (imm-val? 1) #t)
(test (imm-val? 2) #t)
(test (imm-val? 3) #t)
(test (imm-val? 4) #t)
(test (imm-val? 5) #f)

(test (imm-loc->value 4) '())
(test (imm-loc->value 5) #f)
(test (imm-loc->value 6) #t)
(test (imm-loc->value 7) 0)
(test (imm-loc->value 8) 1)
(test (imm-loc->value 9) 2)
(test (imm-loc->value 10) 3)
(test (imm-loc->value 11) 4)

(test (imm-loc->value (imm-value->loc 0)) 0)
(test (imm-loc->value (imm-value->loc 1)) 1)
(test (imm-loc->value (imm-value->loc 2)) 2)
(test (imm-loc->value (imm-value->loc 3)) 3)
(test (imm-loc->value (imm-value->loc 4)) 4)
(test (imm-loc->value (imm-value->loc #t)) #t)
(test (imm-loc->value (imm-value->loc #f)) #f)
(test (imm-loc->value (imm-value->loc '())) '())


(test (let ([h (make-vector 16 #f)]) 
        (with-heap h (init-allocator) (space-available? 2))) #t)
(test (let ([h (make-vector 16 #f)]) 
        (with-heap h (init-allocator) (space-available? 3))) #f)

(test (let ([h (make-vector 16 #f)])
        (with-heap h (init-allocator)) h)
      (vector 12 12 'reserved 'reserved '() #f #t 0 1 2 3 4 'free 'free 'free 'free))

(test (let ([h (make-vector 16 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 111))
              h))
      (list 12 (vector 12 14 'reserved 'reserved '() #f #t 0 1 2 3 4 'flat 111 'free 'free)))

(test (let ([h (make-vector 16 #f)])
        (list (with-heap h
                         (init-allocator)
                         (end-of-second-semispace))
              h))
      (list 15 (vector 12 12 'reserved 'reserved '() #f #t 0 1 2 3 4 'free 'free 'free 'free)))

;; make sure the last space is wasted
;; and that the end of the second semispace is the same as the case above.
(test (let ([h (make-vector 17 #f)])
        (list (with-heap h
                         (init-allocator)
                         (end-of-second-semispace))
              h))
      (list 15 (vector 12 12 'reserved 'reserved '() #f #t 0 1 2 3 4 'free 'free 'free 'free 'wasted)))

;; allocate a few flats
;; and for no great reason, make sure the 'end-of-current-semispace' is correct
(test (let ([h (make-vector 26 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 42)
                         (gc:alloc-flat 54)
                         (end-of-current-semispace))
              h))
      (list 18 (vector 12 16 'reserved 'reserved '() #f #t 0 1 2 3 4 
                       ; space 1
                       'flat 42 'flat 54 'free 'free 'free
                       ; space 2
                       'free 'free 'free 'free 'free 'free 'free)))

;; allocate a few flats and make sure the correct
;; amount of space is availabe.
(test (let ([h (make-vector 26 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 42)
                         (gc:alloc-flat 54)
                         (space-available? 3))
              h))
      (list #t (vector 12 16 'reserved 'reserved '() #f #t 0 1 2 3 4 
                       ; space 1
                       'flat 42 'flat 54 'free 'free 'free
                       ; space 2
                       'free 'free 'free 'free 'free 'free 'free)))

;; fill up the current semi-space
(test (let ([h (make-vector 26 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 42)
                         (gc:alloc-flat 54)
                         (gc:cons 12 14))
              h))
      (list 16 (vector 12 19 'reserved 'reserved '() #f #t 0 1 2 3 4 
                       ; space 1
                       'flat 42 'flat 54 'pair 12 14 
                       ; space 2
                       'free 'free 'free 'free 'free 'free 'free)))

;; we've used up all of the current semi-space. 
;; make sure space-available? returns false
(test (let ([h (make-vector 26 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 42)
                         (gc:alloc-flat 54)
                         (gc:cons 12 14)
                         (space-available? 2))
              h))
      (list #f (vector 12 19 'reserved 'reserved '() #f #t 0 1 2 3 4 
                       ; space 1
                       'flat 42 'flat 54 'pair 12 14 
                       ; space 2
                       'free 'free 'free 'free 'free 'free 'free)))

;; we've used up all of the current semi-space. 
;; now collect-garbage, using only the first flat as a root
;; the first half should all be free
;; the second half should contain the flat, only. 
(test (let ([h (make-vector 26 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:alloc-flat 42)
                   (gc:alloc-flat 54)
                   (gc:cons 12 14)
                   (collect-garbage '(12)))
        h)
      (vector 19 21 'reserved 'reserved '() #f #t 0 1 2 3 4 
              ; space 1
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              ; space 2
              'flat 42 'free 'free 'free 'free 'free))

;; don't use up all the space, but still collect anyway
;; this is just a simple case for one flat and one cons
;; collect-garbage, using only the cons as a root
;; the first half should all be free
;; the second half should contain the cons and flat.
(test (let ([h (make-vector 26 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:alloc-flat 42)
                   (gc:cons 12 12)
                   (collect-garbage '(14)))
        h)
      (vector 19 24 'reserved 'reserved '() #f #t 0 1 2 3 4 
              ; space 1
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              ; space 2
              'pair 22 22 'flat 42 'free 'free))

;; don't use up all the space, but still collect anyway
;; this case uses one cons that references itself.
;; collect-garbage, using only the cons as a root
;; the first half should all be free
;; the second half should contain the cons.
(test (let ([h (make-vector 26 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:cons 12 12)
                   (collect-garbage '(12)))
        h)
      (vector 19 22 'reserved 'reserved '() #f #t 0 1 2 3 4
              ; space 1
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              ; space 2
              'pair 19 19 'free 'free 'free 'free))

;; just a quick test to make sure the root set
;; is empty after i do all this stuff.
;; makes me feel safer about the tests im about to write.
(test (let ([h (make-vector 26 #f)])
        (list (with-heap h
                   (init-allocator)
                   (gc:alloc-flat 42)
                   (gc:alloc-flat 54)
                   (gc:cons 12 14)
                   (get-root-set))
        h))
      (list '() 
            (vector 12 19 'reserved 'reserved '() #f #t 0 1 2 3 4
                    ; space 1
                    'flat 42 'flat 54 'pair 12 14
                    ; space 2
                    'free 'free 'free 'free 'free 'free 'free)))

;; i'd like to avoid having to call collect-garbage myself here in the the test
;; and just call gc:alloc-flat again. this way i could be assured that my 
;; gc:alloc-flat is properly calling collect-garbage.
;; however, as i proved above, get-root-set returns the empty list
;; so i'm not allowed to save any objects in my heap, moving them to the other side.
;; so instead, i have to call it manually, telling it which objects i want to keep
;; i'm a bit uneasy about that, but i think i can find ways to 
;; prove that allocation is forcing gc. 
(test (let ([h (make-vector 26 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:alloc-flat 42)
                   (gc:alloc-flat 54)
                   (gc:cons 12 14)
                   (collect-garbage '(12))
                   (gc:alloc-flat 99)
        h))
      (vector 19 23 'reserved 'reserved '() #f #t 0 1 2 3 4
              ; space 1
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              ; space 2
              'flat 42 'flat 99 'free 'free 'free))


;(test (with-heap (make-vector 10 #f)
;                 (init-allocator)
;                 (gc:alloc-flat 111)
;                 (gc:deref 4))
;      111)