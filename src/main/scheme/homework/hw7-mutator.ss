#lang planet plai/plai:1:19/mutator

(allocator-setup "mark-and-sweep.ss" 100)

(printf "------------\n")

(define (count-down n)
  (cond
    [(zero? n) (count-down 20)]
    [else (count-down (- n 1))]))
(count-down 0)