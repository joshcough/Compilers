#lang planet plai/plai:1:20/collector
(print-only-errors #t)

;; heap
;; 0 : pointer to start of active semispace
;; 1 : allocation pointer to next free space in the active semispace
;; 2 : pointer used in GC
;; 3 : ()
;; 4 : #f
;; 5 : #t
;; 6 : 0
;; 7 : 1
;; 8 : 2
;; 9 : 3
;; 10 : 4
;; 11 : 5
;; 12 : start of first semispace
;; 12 ... (12 + (N-12)/2) - 1 : end of first semispace
;; 12 + (N-12)/2 : start of second semispace
;; (N-1 if N is even)(N-2 if n is odd) :  end of second semispace
;; N-1 - end of heap

;; init-allocator : -> void
(define (init-allocator)
  ; 16 at least allows us to alloc two flats.
  (when (< (heap-size) 16) (error "heap too small"))
  ; the pointer to the currently active semi-space is at position 0
  ; this will toggle between 12 (start of first semi-space) and
  ; the start of the second semispace
  (set-current-semispace (start-of-first-semispace))
  ; this pointer will be used during GC. 
  (heap-set! 2 'reserved)
  (put-constants-in-heap)
  (for ([i (in-range reserved-spaces-at-start-of-heap (heap-size))])
    (heap-set! i 'free))
  (unless (even? (heap-size)) (heap-set! (- (heap-size) 1) 'wasted)))

(define (put-constants-in-heap)
  (heap-set! 3 '())
  (heap-set! 4 #f)
  (heap-set! 5 #t)
  ; put constants [0-5] in the heap
  (for ([i (in-range 0 6)]) 
    ; 7 for the 4 pointers, and then () #f #t
    (heap-set! (+ i 6) i)))

; 3 pointers, '() #t #f and 0-5
(define reserved-spaces-at-start-of-heap 12)

; first semispace starts right after the pointers and constants
; i could have make this a constant and not a function call
; but made it a function so that the code would look more consistent
; if i cant have abstraction, i can at least try to make the code look nice :)
(define (start-of-first-semispace) reserved-spaces-at-start-of-heap)

; just as it says.
(define (semi-space-size) 
  (/ (- (heap-size) reserved-spaces-at-start-of-heap) 2))

;; start-of-second-semispace : -> number
;; the address of 2nd half of the heap
(define (start-of-second-semispace) 
  (+ (start-of-first-semispace) (semi-space-size)))

;; end-of-first-semispace : -> number
;; the address of 2nd half of the heap - 1
(define (end-of-first-semispace) (- (start-of-second-semispace) 1))

;; end-of-second-semispace : -> number
;; end of the heap if heap size is even, one less than that if odd.
(define (end-of-second-semispace) 
  (if (even? (heap-size)) (- (heap-size) 1) (- (heap-size) 2)))

;; utility function that returns end-of-first-semispace, 
;; or end-of-second-semispace, depending on which is active
(define (end-of-current-semispace)
  (if (eq? (get-current-semispace) (start-of-first-semispace)) 
      (end-of-first-semispace)
      (end-of-second-semispace)))

;; change the semispace pointer at the start of the heap
;; to point to the non-active semispace, making it active.
;; i actually think i could have done without this function entirely,
;; and done without that first pointer too,
;; based on where the allocation pointer is, you should know
;; which semispace is active...
(define (switch-semispace)
  (set-current-semispace 
   (if (eq? (get-current-semispace) (start-of-first-semispace)) 
       (start-of-second-semispace)
       (start-of-first-semispace))))

;; the pointer to the currently active semispace
(define (get-current-semispace) (heap-ref 0))

;; set-current-semispace : -> void
;; sets the first and second pointers in the heap
;; to loc. the second is the alloction pointer
;; which will move on allocations. 
;; the first only moves on GC's
(define (set-current-semispace loc) 
  (heap-set! 0 loc)
  (set-allocation-pointer loc))

;; get-other-semispace : -> num
;; get the pointer to the start of the non active semispace
(define (get-other-semispace) 
  (if (eq? (get-current-semispace) (start-of-first-semispace)) 
       (start-of-second-semispace)
       (start-of-first-semispace)))

;; the allocation pointer
(define (get-allocation-pointer) (heap-ref 1))
(define (set-allocation-pointer loc) (heap-set! 1 loc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; immediate values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; i like to call them constants for some reason...

(define (imm-loc? loc) 
  (and (> loc 2)(< loc reserved-spaces-at-start-of-heap)))

(define (imm-loc->value loc)
  (case loc
    [(3) '()]
    [(4) #f]
    [(5) #t]
    [else (- loc 6)]))

(define (imm-val? fv)
  (or (and (exact-integer? fv) 
           (<= fv 5))
      (case fv
        [(()) #t]
        [(#f) #t]
        [(#t) #t]
        [else #f])))

(define (imm-value->loc fv)
  (case fv
    [(()) 3]
    [(#f) 4]
    [(#t) 5]
    [else (+ fv 6)]))

;; gc:deref : loc -> heap-value
;; must signal an error if fl-loc doesn't point to a flat value
(define (gc:deref fl-loc) 
  (cond
    [(imm-loc? fl-loc) (heap-ref fl-loc)]
    [else (unless (gc:flat? fl-loc) (error 'gc:deref "not flat"))
          (heap-ref (+ 1 fl-loc))]))

;; space-available? : -> boolean
;; true if there are n spaces available in the active semispace
;; false otherwise
(define (space-available? n)
  (<= (+ n (get-allocation-pointer))(+ 1 (end-of-current-semispace))))

;; gc:alloc-flat : heap-value -> loc
;; allocate a flat. 
;; if fv is an immediate value then just return the location of that immediate.
;; GC if active semispace is full (less than 2 spaces available)
;; error if active semispace is still full after gc
;; (it would be the other semispace, in that case)
;; finally, return the pointer to the flat.
(define (gc:alloc-flat fv)
  (cond
    [(imm-val? fv) (imm-value->loc fv)]
    [else
     (if (space-available? 2)
         (really-alloc-flat fv)
         (begin 
           (if (procedure? fv)
               (collect-garbage-with-proc-root fv)
               (collect-garbage-with-no-extra-roots))
           (if (space-available? 2)
               (really-alloc-flat fv)
               (error "out of memory"))))]))

;; allocate a flat without checking if space is available
;; its assumed that the checking has already happened. 
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
        (collect-garbage-with-cons-roots hd tl)
        (if (space-available? 3)
            ;hd and tl will now be at the front of the new semispace
            ;but only if they are not immediate values
            ;so some care has to be taken to get the right addresses
            (really-alloc-cons
             (get-after-gc-cons-hd-pointer hd)
             (get-after-gc-cons-tl-pointer hd tl))
            (error "out of memory")))))

; will return either the location of an immediate
; or the address of the to-space 
;(thats where the obj will be if its not an immediate)
(define (get-after-gc-cons-hd-pointer hd)
  (if (imm-loc? hd) hd (get-current-semispace)))

(define (get-after-gc-cons-tl-pointer hd tl)
  (if (imm-loc? tl) tl
      (let ([loc (get-current-semispace)])
        (cond
          ; if head is an immediate, then tl will be at start of to-space
          [(imm-loc? hd) loc]
          ; if head was a flat or cons, tl will be right after it
          [(gc:flat? loc) (+ 2 loc)]
          [(gc:cons? loc) (+ 3 loc)]
          [else (error "how did we get here")]))))

;; allocate a cons without checking if space is available
;; its assumed that the checking has already happened. 
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
  (unless (gc:cons? pr-loc)(error 'gc:first "not a pair"))
  (heap-ref (+ 1 pr-loc)))

;; gc:rest : loc -> loc
;; must signal an error of pr-loc does not point to a pair
(define (gc:rest pr-loc)
  (unless (gc:cons? pr-loc)(error 'gc:first "not a pair"))
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
;; cheney gc                                                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 4 functions here because the GC cant allocate. 
(define (collect-garbage-with-proc-root proc)
  (switch-semispace)
  (for-each copy-root (procedure-roots proc))
  (finish-collecting))

(define (collect-garbage-with-no-extra-roots)
  (switch-semispace)
  (finish-collecting))

(define (collect-garbage-with-cons-roots hd tl)
  (switch-semispace)
  (copy-root hd)
  (copy-root tl)
  (finish-collecting))

; extra roots is a list
; this is legal because its only called during testing
(define (collect-garbage-for-testing extra-roots)
  (switch-semispace)
  (for-each copy-root extra-roots)
  (finish-collecting))

(define (finish-collecting) 
  ; copy the roots from get-root-set
  (for-each copy-root (get-root-set))
  ; after copying all the roots, traverse them to copy children
  ; update pointers, etc. 
  (traverse-to-space)
  ; cleanup the from space, setting everything to 'freed
  ; might not need to do this IRL, but it makes testing easier. :)
  (cleanup-from-space))

; copies a root to the to-space
; and calls set-root! on it, with its new location
(define (copy-root root)
  ; if statement here just so i can do testing using things
  ; that arent really plai roots but are just pointers to cons and flats.
  (if (root? root)
      ; check to see if we've already moved this root.
      ; if it is, do nothing. if its not, copy it.
      (unless (in-current-semispace (read-root root))
        (let ([new-loc (copy-obj (read-root root))]) 
          (set-root! root new-loc)
          new-loc))
      ; this is the testing path. the root is just a cons or a flat.
      (copy-obj root)))

; in-current-semispace: loc -> boolean
(define (in-current-semispace loc)
  (if (eq? (get-current-semispace) (start-of-first-semispace)) 
      (< loc (start-of-second-semispace))
      (>= loc (start-of-second-semispace))))

; set everything in the now inactive semispace to 'freed
(define (cleanup-from-space)
  (let ([loc (get-other-semispace)])
    (for ([i (in-range loc (+ loc (semi-space-size)))])(heap-set! i 'freed))))

(define (traverse-to-space)
  (heap-set! 2 (get-current-semispace))
  (do () 
    ; until the slider reaches the alloction pointer
    ((eq? (heap-ref 2) (heap-ref 1))) 
    (copy-children (heap-ref 2))))

; if the object is a flat or a cons, 
;   copy the object from the from-space to the to-space
;   mark the object as forwarded and leave its forwarding address
; a cons pointers will still point back to the from space
; finally, regardless of object type, return its new loction in the from space
(define (copy-obj loc)
    (cond
    [(imm-loc? loc) loc]
    [(gc:flat? loc) 
     ; we know the space is available in the to-space, so just alloc.
     ; this sticks the object at the end of the to-space
     (let ([new-loc (really-alloc-flat (gc:deref loc))])
       ; leave the forwarding address in the from-space
       (heap-set! loc 'flat-forward)
       (heap-set! (+ 1 loc) new-loc) 
       new-loc)]
    [(gc:cons? loc) 
     ; same as above,
     ; we know the space is available in the to-space, so just alloc.
     (let ([new-loc 
            (really-alloc-cons (heap-ref (+ 1 loc))(heap-ref (+ 2 loc)))])
       ; leave the forwarding address in the from-space
       (heap-set! loc 'cons-forward)
       ; leave it in the first space in the head of the cons
       (heap-set! (+ 1 loc) new-loc)
       ; mark the tail of the cons as freed
       (heap-set! (+ 2 loc) 'freed)
       new-loc )]
    ; just need the forwarding address if the obj has already been moved
    [(eq? 'flat-forward (heap-ref loc)) (heap-ref (+ 1 loc))]
    [(eq? 'cons-forward (heap-ref loc)) (heap-ref (+ 1 loc))]
    ; its a bug if its not a flat, cons or a forwarded flat or cons. 
    [else (begin (printf "loc: ~s heap-ref:~s\n" loc (heap-ref loc))
                 (error "how did we get here?"))]))

; when traversing the to-space, we need to copy children to the to-space. 
(define (copy-children loc)
  (cond
    ; the only flat with children is a proc. 
    ; if we have a proc, copy all of its children.
    [(gc:flat? loc) 
     (let ([v (heap-ref (+ 1 loc))])
       (if (procedure? v) 
           (for-each copy-root (procedure-roots v))
           (void)))
     ; bump the magic pointer by two (the size of a flat)
     (heap-set! 2 (+ 2 (heap-ref 2)))]
    ; cons have hd and tl children that need copying. 
    [(gc:cons? loc) 
     (heap-set! (+ 1 loc) (copy-obj (heap-ref (+ 1 loc))))
     (heap-set! (+ 2 loc) (copy-obj (heap-ref (+ 2 loc))))
     ; bump the magic pointer by two (the size of a flat)
     (heap-set! 2 (+ 3 (heap-ref 2)))]
    [else (begin (printf "loc: ~s heap-ref:~s\n" loc (heap-ref loc))
                 (error "how did we get here?"))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests                                                                     ;
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

;; immediate loc tests
(test (imm-loc? 2) #f)
(test (imm-loc? 3) #t)
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
(test (imm-val? 5) #t)
(test (imm-val? 6) #f)

(test (imm-loc->value 3) '())
(test (imm-loc->value 4) #f)
(test (imm-loc->value 5) #t)
(test (imm-loc->value 6) 0)
(test (imm-loc->value 7) 1)
(test (imm-loc->value 8) 2)
(test (imm-loc->value 9) 3)
(test (imm-loc->value 10) 4)
(test (imm-loc->value 11) 5)

(test (imm-loc->value (imm-value->loc 0)) 0)
(test (imm-loc->value (imm-value->loc 1)) 1)
(test (imm-loc->value (imm-value->loc 2)) 2)
(test (imm-loc->value (imm-value->loc 3)) 3)
(test (imm-loc->value (imm-value->loc 4)) 4)
(test (imm-loc->value (imm-value->loc 5)) 5)
(test (imm-loc->value (imm-value->loc #t)) #t)
(test (imm-loc->value (imm-value->loc #f)) #f)
(test (imm-loc->value (imm-value->loc '())) '())

(test (with-heap (make-vector 16 #f) (init-allocator)(gc:deref 3)) '())
(test (with-heap (make-vector 16 #f) (init-allocator)(gc:deref 4)) #f)
(test (with-heap (make-vector 16 #f) (init-allocator)(gc:deref 5)) #t)
(test (with-heap (make-vector 16 #f) (init-allocator)(gc:deref 6)) 0)
(test (with-heap (make-vector 16 #f) (init-allocator)(gc:deref 7)) 1)
(test (with-heap (make-vector 16 #f) (init-allocator)(gc:deref 8)) 2)
(test (with-heap (make-vector 16 #f) (init-allocator)(gc:deref 9)) 3)
(test (with-heap (make-vector 16 #f) (init-allocator)(gc:deref 10)) 4)
(test (with-heap (make-vector 16 #f) (init-allocator)(gc:deref 11)) 5)

; gc:deref, flat?, cons?, first, rest, set-first!, set-rest! tests
(test (with-heap 
       (make-vector 16 #f) 
       (init-allocator)
       (gc:alloc-flat 111)
       (gc:deref 12)) 111)

(test (with-heap 
       (make-vector 16 #f) 
       (init-allocator)
       (gc:alloc-flat 111)
       (gc:flat? 12)) #t)

(test (with-heap 
       (make-vector 16 #f) 
       (init-allocator)
       (gc:alloc-flat 111)
       (gc:cons? 12)) #f)

(test (with-heap 
       (make-vector 18 #f) 
       (init-allocator)
       (gc:cons 4 5)
       (gc:flat? 12)) #f)

(test (with-heap 
       (make-vector 18 #f) 
       (init-allocator)
       (gc:cons 4 5)
       (gc:cons? 12)) #t)

(test/exn (with-heap 
       (make-vector 18 #f) 
       (init-allocator)
       (gc:cons 4 5)
       (gc:deref 12)) "not flat")

(test/exn (with-heap 
       (make-vector 16 #f) 
       (init-allocator)
       (gc:alloc-flat 111)
       (gc:first 12)) "not a pair")

(test/exn (with-heap 
       (make-vector 16 #f) 
       (init-allocator)
       (gc:alloc-flat 111)
       (gc:rest 12)) "not a pair")

(test (with-heap 
       (make-vector 18 #f) 
       (init-allocator)
       (gc:cons 4 5)
       (gc:first 12)) 4)

(test (with-heap 
       (make-vector 18 #f) 
       (init-allocator)
       (gc:cons 4 5)
       (gc:rest 12)) 5)

(test (with-heap 
       (make-vector 18 #f) 
       (init-allocator)
       (gc:cons 4 5)
       (gc:set-first! 12 5)
       (gc:set-rest! 12 4)
       (list (gc:first 12)(gc:rest 12))) '(5 4))

(test/exn (with-heap 
       (make-vector 16 #f) 
       (init-allocator)
       (gc:alloc-flat 111)
       (gc:set-first! 12 7)) "not a pair")

(test/exn (with-heap 
       (make-vector 16 #f) 
       (init-allocator)
       (gc:alloc-flat 111)
       (gc:set-rest! 12 7)) "not a pair")

; with heap size 16, we have a semispace size of 2. 
; make sure space-available 2 is true, and 3 is false.
(test (let ([h (make-vector 16 #f)]) 
        (with-heap h (init-allocator) (space-available? 2))) #t)
(test (let ([h (make-vector 16 #f)]) 
        (with-heap h (init-allocator) (space-available? 3))) #f)

; make sure after init-allocator that our heap is good
(test (let ([h (make-vector 16 #f)])
        (with-heap h (init-allocator)) h)
      (vector
       ; the 3 pointers
       12 12 'reserved
       ; the constants
       '() #f #t 0 1 2 3 4 5
       ; the first semispace
       'free 'free
       ; the second semispace
       'free 'free))

; gc:alloc-flat test
(test (let ([h (make-vector 16 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 111))
              h))
      (list 12 (vector 12 14 'reserved '() #f #t 0 1 2 3 4 5 'flat 111 'free 'free)))

(test (let ([h (make-vector 16 #f)])
        (list (with-heap h (init-allocator) (end-of-second-semispace)) h))
      (list 15 
            (vector 12 12 'reserved '() #f #t 0 1 2 3 4 5
                    'free 'free 
                    'free 'free)))

;; make sure the last space is wasted
;; and that the end of the second semispace is the same as the case above.
(test (let ([h (make-vector 17 #f)])
        (list (with-heap h (init-allocator) (end-of-second-semispace)) h))
      (list 15 (vector 12 12 'reserved '() #f #t 0 1 2 3 4 5
                       'free 'free 
                       'free 'free 'wasted)))

;; allocate a few flats
;; and for no great reason, make sure the 'end-of-current-semispace' is correct
(test (let ([h (make-vector 26 #f)])
        (list (with-heap h
                         (init-allocator)
                         (gc:alloc-flat 42)
                         (gc:alloc-flat 54)
                         (end-of-current-semispace))
              h))
      (list 18 (vector 12 16 'reserved '() #f #t 0 1 2 3 4 5
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
      (list #t (vector 12 16 'reserved '() #f #t 0 1 2 3 4 5
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
      (list 16 (vector 12 19 'reserved '() #f #t 0 1 2 3 4 5
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
      (list #f (vector 12 19 'reserved '() #f #t 0 1 2 3 4 5
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
                   (collect-garbage-for-testing '(12)))
        h)
      (vector 19 21 21 '() #f #t 0 1 2 3 4 5
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
                   (collect-garbage-for-testing '(14)))
        h)
      (vector 19 24 24 '() #f #t 0 1 2 3 4 5
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
                   (collect-garbage-for-testing '(12)))
        h)
      (vector 19 22 22 '() #f #t 0 1 2 3 4 5
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
            (vector 12 19 'reserved '() #f #t 0 1 2 3 4 5
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
                   (collect-garbage-for-testing '(12))
                   (gc:alloc-flat 99)
        h))
      (vector 19 23 21 '() #f #t 0 1 2 3 4 5
              ; space 1
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              ; space 2
              'flat 42 'flat 99 'free 'free 'free))

; the GCer reorders things, roots first. test it.
(test (let ([h (make-vector 40 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:alloc-flat 42) ; 12 -> 34
                   (gc:alloc-flat 54) ; 14 -> 36
                   (gc:alloc-flat 66) ; 16 -> 32
                   (gc:cons 16 12)    ; 18 -> 26
                   (gc:alloc-flat 78) ; 21 -> 38
                   (gc:cons 14 21)    ; 23 -> 29
                   ; keep the two cons as roots.
                   (collect-garbage-for-testing '(18 23))
        h))
      (vector 26 40 40 '() #f #t 0 1 2 3 4 5
              ; space 1
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              ; space 2
              'pair 32 34 'pair 36 38 'flat 66 'flat 42 'flat 54 'flat 78))


; test the same as above, accept have the second cons 
; reference one of the flats referenced by the first cons
; this tests forwarding addresses being put in the from-space.
(test (let ([h (make-vector 40 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:alloc-flat 42) ; 12 -> 34
                   (gc:alloc-flat 54) ; 14 -> 36
                   (gc:alloc-flat 66) ; 16 -> 32
                   (gc:cons 16 12)    ; 18 -> 26
                   (gc:alloc-flat 78) ; 21 -> DEAD
                   (gc:cons 14 12)    ; 23 -> 29
                   ; keep the two cons as roots.
                   (collect-garbage-for-testing '(18 23))
        h))
      (vector 26 38 38 '() #f #t 0 1 2 3 4 5
              ; space 1
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              ; space 2
              'pair 32 34 'pair 36 34 'flat 66 'flat 42 'flat 54 'free 'free))

; forwarding should still work if cons reference each other
(test (let ([h (make-vector 40 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:cons 15 15)    
                   (gc:cons 12 12)    
                   (collect-garbage-for-testing '(15 12))
        h))
      (vector 26 32 32 '() #f #t 0 1 2 3 4 5
              ; space 1
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              ; space 2
              'pair 29 29 'pair 26 26 
              'free 'free 'free 'free 'free 'free 'free 'free))

; fill up the first space, then alloc a flat.
; the flat should be the only thing in the to-space
(test (let ([h (make-vector 40 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:alloc-flat 42) ; 12 -> DEAD
                   (gc:alloc-flat 54) ; 14 -> DEAD
                   (gc:alloc-flat 66) ; 16 -> DEAD
                   (gc:cons 16 12)    ; 18 -> DEAD
                   (gc:alloc-flat 78) ; 21 -> DEAD
                   (gc:cons 14 21)    ; 23 -> DEAD
                   (gc:alloc-flat 99) ; The guy that triggers the gc)
        h))
      (vector 26 28 26 '() #f #t 0 1 2 3 4 5
              ; space 1
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              ; space 2
              'flat 99 'free 'free 'free 'free 'free
              'free 'free 'free 'free 'free 'free 'free))

; fill up the first space, then alloc a cons.
; the hd and tl should be the first things in the to-space
; followed by the cons itself.
(test (let ([h (make-vector 40 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:alloc-flat 42) ; 12 -> DEAD
                   (gc:alloc-flat 54) ; 14 -> 28
                   (gc:alloc-flat 66) ; 16 -> 26
                   (gc:cons 16 12)    ; 18 -> DEAD
                   (gc:alloc-flat 78) ; 21 -> DEAD
                   (gc:cons 14 21)    ; 23 -> DEAD
                   (gc:cons 16 14)    ; The guy that triggers the gc)
        h))
      (vector 26 33 30 '() #f #t 0 1 2 3 4 5
              ; space 1
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              ; space 2
              'flat 66 'flat 54 'pair 26 28
              'free 'free 'free 'free 'free 'free 'free))

; do the same thing as the last test, but have the new cons
; reference immediate values instead of flats in the semispace.
; the cons should be the only thing in the heap afterwards
(test (let ([h (make-vector 40 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:alloc-flat 42) ; 12 -> DEAD
                   (gc:alloc-flat 54) ; 14 -> DEAD
                   (gc:alloc-flat 66) ; 16 -> DEAD
                   (gc:cons 16 12)    ; 18 -> DEAD
                   (gc:alloc-flat 78) ; 21 -> DEAD
                   (gc:cons 14 21)    ; 23 -> DEAD
                   (gc:cons 6 7)    ; The guy that triggers the gc)
        h))
      (vector 26 29 26 '() #f #t 0 1 2 3 4 5
              ; space 1
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              ; space 2
              'pair 6 7 'free 'free 'free 'free
              'free 'free 'free 'free 'free 'free 'free))

; just for sanity sake, GC, then GC again.
(test (let ([h (make-vector 40 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:cons 15 15)    
                   (gc:cons 12 12)    
                   (collect-garbage-for-testing '(15 12))
                   (collect-garbage-for-testing '(26 29))
        h))
      (vector 12 18 18 '() #f #t 0 1 2 3 4 5
              ; space 1
              'pair 15 15 'pair 12 12 
              'freed 'freed 'freed 'freed 'freed 'freed 'freed 'freed
              ; space 2
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              'freed 'freed 'freed 'freed 'freed 'freed 'freed))

; allocate enough stuff to force two garbage collections.
(test (let ([h (make-vector 40 #f)])
        (with-heap h
                   (init-allocator)
                   (gc:alloc-flat 42) ; 12 -> DEAD
                   (gc:alloc-flat 54) ; 14 -> 28
                   (gc:alloc-flat 66) ; 16 -> 26
                   (gc:cons 16 12)    ; 18 -> DEAD
                   (gc:alloc-flat 78) ; 21 -> DEAD
                   (gc:cons 14 21)    ; 23 -> DEAD
                   (gc:cons 16 14)    ; The guy that triggers the first gc. 30 -> 12
                   (gc:alloc-flat 78) ; 33 -> DEAD 
                   (gc:alloc-flat 78) ; 35 -> DEAD
                   (gc:alloc-flat 78) ; 37 -> DEAD
                   ; this guy triggers the second gc.
                   ; he references the cons that triggered the first gc
                   ; and an immediate.
                   (gc:cons 30 6) ; ? 
        h))
      (vector 12 22 19 '() #f #t 0 1 2 3 4 5
              ; space 1
              'pair 15 17 'flat 66 'flat 54 'pair 12 6 'freed 'freed 'freed 'freed
              ; space 2
              'freed 'freed 'freed 'freed 'freed 'freed 'freed
              'freed 'freed 'freed 'freed 'freed 'freed 'freed))

; out of memory test
(test/exn (with-heap 
       (make-vector 16 #f) 
       (init-allocator)
       (gc:cons 4 5)) "out of memory")

; too small!
(test/exn (with-heap 
       (make-vector 10 #f) 
       (init-allocator)
       (gc:cons 4 5)) "heap too small")