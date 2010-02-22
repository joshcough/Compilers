#lang planet plai/plai:1:19/collector
(print-only-errors #t)

;; init-allocator : -> void
(define (init-allocator)
  ; 8 for now, because im not sure yet what i need
  (when (< (heap-size) 8) (error 'too-small))
  ; the pointer to the currently active semi-space is at position 0
  ; this will toggle between 4 (start of first semi-space) and
  ; the start of the second semispace
  (set-current-semispace (start-of-first-semispace))
  ; reserve these guys for now til i figure out what to do with them
  (heap-set! 2 'reserved)
  (heap-set! 3 'reserved)
  (for ([i (in-range 4 (heap-size))])(heap-set! i 'free))
  (unless (even? (heap-size)) (heap-set! (- (heap-size) 1) 'wasted)))

(define reserved-spaces-at-start-of-heap 4)
(define (start-of-first-semispace) reserved-spaces-at-start-of-heap)

(define (semi-space-size) 
  (/ (- (heap-size) reserved-spaces-at-start-of-heap) 2))

;; start-of-second-semispace : -> number
;; the address of 2nd half of the heap
(define (start-of-second-semispace) 
  (+ (start-of-first-semispace) (semi-space-size)))

(define (end-of-first-semispace) (- (start-of-second-semispace) 1))
(define (end-of-second-semispace) (- (heap-size) 1))
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
  (printf "deref : ~s\n" fl-loc) 
  (unless (gc:flat? fl-loc)
    (error 'gc:deref "not flat"))
  (heap-ref (+ 1 fl-loc)))


(define (space-available? n)
  (<= (+ n (get-allocation-pointer))(+ 1 (end-of-current-semispace))))

;; gc:alloc-flat : heap-value -> loc
(define (gc:alloc-flat fv) 
  (if (space-available? 2)
      (really-alloc-flat fv)
      (begin
        (collect-garbage (if (procedure? fv)
                             (map read-root (procedure-roots fv))
                             '()))
        (if (space-available? 2)
            (really-alloc-flat fv)
            (error "out of memory")))))

(define (really-alloc-flat fv)
  (let ([n (get-allocation-pointer)])
        (heap-set! n 'flat)
        (heap-set! (+ 1 n) fv)
        (set-allocation-pointer (+ 2 n))
        n))

;; gc:cons : loc loc -> loc
;; hd and tl are guaranteed to have been earlier
;; results from either gc:alloc-flat or gc:cons
(define (gc:cons hd tl) 
    (if (space-available? 3)
        (really-alloc-cons hd tl)
        (begin
        (collect-garbage '(hd tl))
        (if (space-available? 3)
            (really-alloc-cons hd tl)
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
(define (gc:flat? loc) (eq? (heap-ref loc) 'flat))

;; gc:cons? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:cons? loc) (eq? (heap-ref loc) 'pair))

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
    (printf "collecting garbage: ~s ~s\n" (get-root-set) extra-roots) 
    (switch-semispace)
    ; change these lambdas to a top level proc
    (for-each (λ (root) (move-obj (read-root root))) (get-root-set))
    (for-each (λ (root) (move-obj root)) extra-roots)
    (cleanup-semispace (get-other-semispace))))

(define (cleanup-semispace loc)
  (for ([i (in-range loc (+ loc (semi-space-size)))])(heap-set! i 'free)))

(define (move-obj loc)
  (cond
    [(gc:flat? loc) (move-flat loc)]
    [(gc:cons? loc) (move-cons loc)]
    [(eq? 'flat-forward (heap-ref loc)) (heap-ref (+ 1 loc))]
    [(eq? 'cons-forward (heap-ref loc)) (heap-ref (+ 1 loc))]
    [else (begin (printf "loc: ~s\n" loc)
                 (printf "heap-ref loc: ~s\n" (heap-ref loc)) 
                 (error "how did we get here?"))]))

(define (move-flat loc)
  (printf "moving flat: ~s ~s\n" loc (heap-ref (+ 1 loc))) 
  (let ([v (heap-ref (+ 1 loc))])
    (if (procedure? v) 
        (move-proc loc v)
        (let ([new-loc (gc:alloc-flat (gc:deref loc))])
          (heap-set! loc 'flat-forward)
          (heap-set! (+ 1 loc) new-loc) new-loc))))

; move proc must move all the things the proc points to. 
; but how the heck do i make the procedure point to new roots?
; answer: use set-root!
(define (move-proc loc v) 7)
;  (print "move-proc!")
;  (let ([new-loc (gc:alloc-flat (gc:deref loc))])
;    (heap-set! loc 'flat-forward)
;    ; make the lambda top level.
;    (for-each (λ (root) (move-obj root)) (procedure-roots v)))

(define (move-cons loc)
  (printf "move cons: ~s\n" loc) 
  (heap-set! loc 'cons-forward)
  (let ([new-loc (gc:cons (heap-ref (+ 1 loc))(heap-ref (+ 2 loc)))])
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

(test (with-heap (make-vector 8 #f) (end-of-first-semispace)) 5)
(test (with-heap (make-vector 8 #f) (end-of-second-semispace)) 7)

(test (with-heap (make-vector 10 #f) (end-of-first-semispace)) 6)
(test (with-heap (make-vector 10 #f) (end-of-second-semispace)) 9)

(test (with-heap (make-vector 16 #f) (end-of-first-semispace)) 9)
(test (with-heap (make-vector 16 #f) (end-of-second-semispace)) 15)

(test (with-heap (make-vector 18 #f) (end-of-first-semispace)) 10)
(test (with-heap (make-vector 18 #f) (start-of-second-semispace)) 11)
(test (with-heap (make-vector 18 #f) (end-of-second-semispace)) 17)

(test (with-heap (make-vector 64 #f) (start-of-first-semispace)) 4)
(test (with-heap (make-vector 64 #f) (end-of-first-semispace)) 33)
(test (with-heap (make-vector 64 #f) (start-of-second-semispace)) 34)
(test (with-heap (make-vector 64 #f) (end-of-second-semispace)) 63)

(test (with-heap (make-vector 100 #f) (start-of-first-semispace)) 4)
(test (with-heap (make-vector 100 #f) (end-of-first-semispace)) 51)
(test (with-heap (make-vector 100 #f) (start-of-second-semispace)) 52)
(test (with-heap (make-vector 100 #f) (end-of-second-semispace)) 99)

(test (let ([h (make-vector 10 #f)]) 
        (with-heap h (init-allocator) (space-available? 3))) #t)
(test (let ([h (make-vector 10 #f)]) 
        (with-heap h (init-allocator) (space-available? 4))) #f)

(test (let ([h (make-vector 10 #f)])
        (with-heap h (init-allocator)) h)
      (vector 4 4 'reserved 'reserved 'free 'free 'free 'free 'free 'free))

(test (let ([h (make-vector 10 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 111))
              h))
      (list 4 (vector 4 6 'reserved 'reserved 'flat 111 'free 'free 'free 'free)))

; allocate a few flats
; and for no great reason, make sure the 'end-of-current-semispace' is correct
(test (let ([h (make-vector 18 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 42)
                         (gc:alloc-flat 54)
                         (end-of-current-semispace))
              h))
      (list 10 (vector 4 8 'reserved 'reserved
                       ; space 1
                       'flat 42 'flat 54 'free 'free 'free
                       ; space 2
                       'free 'free 'free 'free 'free 'free 'free)))

; allocate a few flats and make sure the correct
; amount of space is availabe.
(test (let ([h (make-vector 18 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 42)
                         (gc:alloc-flat 54)
                         (space-available? 3))
              h))
      (list #t (vector 4 8 'reserved 'reserved 'flat 42 
                       'flat 54 'free 'free 'free 
                       'free 'free 'free 'free 'free 'free 'free)))

; fill up the current semi-space
(test (let ([h (make-vector 18 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 42)
                         (gc:alloc-flat 54)
                         (gc:cons 4 6))
              h))
      (list 8 (vector 4 11 'reserved 'reserved 
                      ; space 1
                      'flat 42 'flat 54 'pair 4 6 
                      ; space 2
                      'free 'free 'free 'free 'free 'free 'free)))

; we've used up all of the current semi-space. 
; make sure space-available? returns false
(test (let ([h (make-vector 18 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 42)
                         (gc:alloc-flat 54)
                         (gc:cons 4 6)
                         (space-available? 2))
              h))
      (list #f (vector 4 11 'reserved 'reserved 
                       ; space 1
                       'flat 42 'flat 54 'pair 4 6 
                       ; space 2
                       'free 'free 'free 'free 'free 'free 'free)))

; we've used up all of the current semi-space. 
; now collect-garbage, using only the first flat as a root
; the first half should all be free
; the second half should contain the flat, only. 
(test (let ([h (make-vector 18 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:alloc-flat 42)
                   (gc:alloc-flat 54)
                   (gc:cons 4 6)
                   (collect-garbage '(4)))
        h)
      (vector 11 13 'reserved 'reserved
              ; space 1
              'free 'free 'free 'free 'free 'free 'free
              ; space 2
              'flat 42 'free 'free 'free 'free 'free))

; don't use up all the space, but still collect anyway
; this is just a simple case for one flat and one cons
; collect-garbage, using only the cons as a root
; the first half should all be free
; the second half should contain the cons and flat.
(test (let ([h (make-vector 18 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:alloc-flat 42)
                   (gc:cons 4 4)
                   (collect-garbage '(6)))
        h)
      (vector 11 16 'reserved 'reserved
              ; space 1
              'free 'free 'free 'free 'free 'free 'free
              ; space 2
              'pair 14 14 'flat 42 'free 'free))

; don't use up all the space, but still collect anyway
; this case uses one cons that references itself.
; collect-garbage, using only the cons as a root
; the first half should all be free
; the second half should contain the cons.
(test (let ([h (make-vector 18 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:cons 4 4)
                   (collect-garbage '(4)))
        h)
      (vector 11 14 'reserved 'reserved
              ; space 1
              'free 'free 'free 'free 'free 'free 'free
              ; space 2
              'pair 11 11 'free 'free 'free 'free))

; just a quick test to make sure the root set
; is empty after i do all this stuff.
; makes me feel safer about the tests im about to write.
(test (let ([h (make-vector 18 #f)])
        (list (with-heap h
                   (init-allocator)
                   (gc:alloc-flat 42)
                   (gc:alloc-flat 54)
                   (gc:cons 4 6)
                   (get-root-set))
        h))
      (list '() 
            (vector 4 11 'reserved 'reserved
                    ; space 1
                    'flat 42 'flat 54 'pair 4 6 
                    ; space 2
                    'free 'free 'free 'free 'free 'free 'free)))

; i'd like to avoid having to call collect-garbage myself here in the the test
; and just call gc:alloc-flat again. this way i could be assured that my 
; gc:alloc-flat is properly calling collect-garbage.
; however, as i proved above, get-root-set returns the empty list
; so i'm not allowed to save any objects in my heap, moving them to the other side.
; so instead, i have to call it manually, telling it which objects i want to keep
; i'm a bit uneasy about that, but i think i can find ways to 
; prove that allocation is forcing gc. 
(test (let ([h (make-vector 18 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:alloc-flat 42)
                   (gc:alloc-flat 54)
                   (gc:cons 4 6)
                   (collect-garbage '(4))
                   (gc:alloc-flat 99)
        h))
      (vector 11 15 'reserved 'reserved
              ; space 1
              'free 'free 'free 'free 'free 'free 'free
              ; space 2
              'flat 42 'flat 99 'free 'free 'free))


(test (with-heap (make-vector 10 #f)
                 (init-allocator)
                 (gc:alloc-flat 111)
                 (gc:deref 4))
      111)