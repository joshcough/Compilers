#lang planet plai/plai:1:19/collector
(print-only-errors #t)

;; init-allocator : -> void
(define (init-allocator)
  ; 6 for now, because im not sure yet what i need
  (when (< (heap-size) 6) (error 'too-small))
  ; the pointer to the currently active semi-space is at position 0
  ; this will toggle between 3? and midpoint
  (set-current-semispace (start-of-first-semispace))
  (for ([i (in-range 2 (heap-size))])(heap-set! i 'free)))

(define number-of-items-at-start-of-heap 2)
(define (start-of-first-semispace) number-of-items-at-start-of-heap)
;; start-of-second-semispace : -> number
;; the address of 2nd half of the heap
(define (start-of-second-semispace) (+ 2 (ceiling (/ (- (heap-size) 2) 2))))

(define (end-of-first-semispace) (- (start-of-second-semispace) 1))
(define (end-of-second-semispace) (- (heap-size) 1))
(define (end-of-current-semispace)
  (if (eq? (get-current-semispace) (start-of-first-semispace)) 
      (end-of-first-semispace)
      (end-of-second-semispace)))

(define (switch-semispace)
  (set-current-semispace 
   (if (eq? (get-current-semispace (start-of-first-semispace))) 
       (start-of-second-semispace)
       (start-of-first-semispace))))

(define (get-current-semispace) (heap-ref 0))
(define (set-current-semispace loc) 
  (heap-set! 0 loc)
  (set-allocation-pointer loc))

(define (get-allocation-pointer) (heap-ref 1))
(define (set-allocation-pointer loc) 
  (heap-set! 1 loc))

;; gc:deref : loc -> heap-value
;; must signal an error if fl-loc doesn't point to a flat value
(define (gc:deref fl-loc) 
  (unless (gc:flat? fl-loc)
    (error 'gc:deref "not flat"))
  (heap-ref (+ 1 fl-loc)))


(define (space-available? n)
  (<= (+ n (get-allocation-pointer))(+ 1 (end-of-current-semispace))))

;; gc:alloc-flat : heap-value -> loc
(define (gc:alloc-flat fv) 
  (if (space-available? 2)
      (let ([n (get-allocation-pointer)])
        (heap-set! n 'flat)
        (heap-set! (+ 1 n) fv)
        (set-allocation-pointer (+ 2 n))
        n
        )
      (error "implement me")))

(define (collect-garbage) (error "out of memory"))
;; gc:cons : loc loc -> loc
;; hd and tl are guaranteed to have been earlier
;; results from either gc:alloc-flat or gc:cons
(define (gc:cons hd tl) 
    (if (space-available? 3)
      (let ([n (get-allocation-pointer)])
        (heap-set! n 'pair)
        (heap-set! (+ 1 n) hd)
        (heap-set! (+ 2 n) tl)
        (set-allocation-pointer (+ 3 n))
        n
        )
      (error "implement me")))

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
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (with-heap (make-vector 6 #f) (start-of-first-semispace)) 2)

(test (with-heap (make-vector 6 #f) (start-of-second-semispace)) 4)
(test (with-heap (make-vector 8 #f) (start-of-second-semispace)) 5)

(test (with-heap (make-vector 6 #f) (end-of-first-semispace)) 3)
(test (with-heap (make-vector 6 #f) (end-of-second-semispace)) 5)

(test (with-heap (make-vector 10 #f) (end-of-first-semispace)) 5)
(test (with-heap (make-vector 10 #f) (end-of-second-semispace)) 9)

(test (with-heap (make-vector 16 #f) (end-of-first-semispace)) 8)
(test (with-heap (make-vector 16 #f) (end-of-second-semispace)) 15)

(test (with-heap (make-vector 100 #f) (start-of-first-semispace)) 2)
(test (with-heap (make-vector 100 #f) (start-of-second-semispace)) 51)
(test (with-heap (make-vector 100 #f) (end-of-first-semispace)) 50)
(test (with-heap (make-vector 100 #f) (end-of-second-semispace)) 99)

(test (let ([h (make-vector 10 #f)]) 
        (with-heap h (init-allocator) (space-available? 2))) #t)

(test (let ([h (make-vector 10 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 111))
              h))
      (list 2 (vector 2 4 'flat 111 'free 'free 'free 'free 'free 'free)))

; use a 16 heap (7 for 2 flats and 1 cons)
(test (let ([h (make-vector 16 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 42)
                         (gc:alloc-flat 54)
                         (end-of-current-semispace))
              h))
      (list 8 (vector 2 6 'flat 42 'flat 54 'free 'free 'free 'free 'free 'free 'free 'free 'free 'free)))

(test (let ([h (make-vector 16 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 42)
                         (gc:alloc-flat 54)
                         (space-available? 3))
              h))
      (list #t (vector 2 6 'flat 42 'flat 54 'free 'free 'free 'free 'free 'free 'free 'free 'free 'free)))

(test (let ([h (make-vector 16 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 42)
                         (gc:alloc-flat 54)
                         (gc:cons 2 4))
              h))
      (list 6 (vector 2 9 'flat 42 'flat 54 'pair 2 4 'free 'free 'free 'free 'free 'free 'free)))

(test (with-heap (make-vector 10 #f)
                 (init-allocator)
                 (gc:alloc-flat 111)
                 (gc:deref 2))
      111)