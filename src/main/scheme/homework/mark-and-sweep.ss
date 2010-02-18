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

(define (start-of-first-semispace) 3)
;; start-of-second-semispace : -> number
;; the address of 2nd half of the heap
(define (start-of-second-semispace) (ceiling (/ (heap-size) 2)))

(define (end-of-first-semispace) (- (start-of-second-semispace) 1))
(define (end-of-second-semispace) (- (heap-size) 1))
(define (end-of-current-semispace)
  (if (eq? (get-current-semispace) (start-of-first-semispace)) 
      (end-of-first-semispace)
      (end-of-second-semispace)))

(test (with-heap (make-vector 6 #f) (end-of-first-semispace)) 2)
(test (with-heap (make-vector 6 #f) (end-of-second-semispace)) 5)

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
(define (gc:deref fl-loc) #t)

(define (space-available? n)
  (< (+ n (get-allocation-pointer))(end-of-current-semispace)))

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
(define (gc:cons hd tl) 0)

;; gc:first : loc -> loc
;; must signal an error of pr-loc does not point to a pair
(define (gc:first pr-loc) 0)

;; gc:rest : loc -> loc
;; must signal an error of pr-loc does not point to a pair
(define (gc:rest pr-loc) 0)

;; gc:flat? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:flat? loc) #t)

;; gc:cons? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:cons? loc) #f)

;; gc:set-first! : loc loc -> void
;; must signal an error of pr-loc does not point to a pair
(define (gc:set-first! pr-loc new) (void))

;; gc:set-rest! : loc loc -> void
;; must signal an error of pr-loc does not point to a pair
(define (gc:set-rest! pr-ptr new) (void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (let ([h (make-vector 6 #f)])
        (with-heap h (init-allocator))
        h)
      (vector 3 3 'free 'free 'free 'free))
;
;
;(test (with-heap (make-vector 6 #f) (start-of-second-semispace))
;      3)
;
;(test (with-heap (make-vector 8 #f) (start-of-second-semispace))
;      4)
;
;(test (let ([h (make-vector 8 #f)])
;        (list (with-heap h
;                         (init-allocator)
;                         (gc:alloc-flat 111))
;              h))
;      (list 2
;            (vector 2 3 111 'free 'free 'free 'free 'free)))