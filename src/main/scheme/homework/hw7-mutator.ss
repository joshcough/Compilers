#lang planet plai/plai:1:19/mutator

(allocator-setup "mark-and-sweep.ss" 200)

(printf "------------\n")

;(define (count-down n)
;  (cond
;    [(zero? n) (count-down 20)]
;    [else (count-down (- n 1))]))
;(count-down 0)

;(define (mk-list n)
;  (cond
;    [(zero? n) '()]
;    [else (cons n (mk-list (- n 1)))]))
;(define (forever)
;  (mk-list 10)
;  (forever))
;(forever)

(define (proc-lst n)
  (cond
    [(zero? n) (lambda () 0)]
    [else (let ([n1 (proc-lst (- n 1))])
            (lambda () (+ (n1) n)))]))
(define (forever)
  ((proc-lst 10))
  (forever))
(forever)
;(proc-lst 10)
;(proc-lst 10)
;(proc-lst 10)
;(proc-lst 10)
;(proc-lst 10)
;(proc-lst 10)
;(proc-lst 10)