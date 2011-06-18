#lang racket/base
(require racket/system
         racket/path
         racket/list)


(define answers-hash (make-hash))

(define (register student fn answer)
  (hash-set! (hash-ref answers-hash student (λ () 
                                              (define h (make-hash))
                                              (hash-set! answers-hash student h)
                                              h))
             fn answer))

(define (build-answers-hash)
  (for ([f (in-directory)])
    (when (regexp-match #rx"[.]L2f$" (path->bytes f))
      (define segs (explode-path f))
      (printf "~a\n" segs)
      (when (>= (length segs) 3)
        (define student (path->string (list-ref (reverse segs) 2)))
        (define base (regexp-replace #rx".L2f$" (path->string (last segs)) ""))
        (define fn (path->string (last segs)))
        (define expected 
          (with-handlers ((exn:fail:read? (λ (x) 'bogus)))
            (call-with-input-file (bytes->path (regexp-replace #rx"L2f$" (path->bytes f) "gres"))
              read)))
        (define got (sys (format "/Users/sjaconette/Documents/s11/hw/graph-color ~a" f)))
        (define color (equal? 'no (sys (format "/Users/sjaconette/Documents/322-interps/check-coloring ~a" (bytes->path (regexp-replace #rx"L2f$" (path->bytes f) "gres"))))))
        #;(printf "trying ~s ...\n~s\n~s \n~s\n" f expected got color) (flush-output)
        (register student base (and (equal? expected got) color))
        (printf "~a\n" (and (equal? expected got) color))
        #;(sys (format "/Users/sjaconette/Documents/s11/hw/graph-color ~a > /Users/sjaconette/Documents/graph-test/robby/graph-test/~a" f (bytes->path (regexp-replace #rx"L2f$" (path->bytes f) "gres"))))
        #;(unless (not (equal? (car segs) 'robby))
            (call-with-output-file (bytes->path (regexp-replace #rx"L2f$" (path->bytes f) "gres"))
          (λ (port)
            (fprintf port "~s" got))
          #:exists 'replace))
        ))))

(struct err (msg))

;; sys : string -> (or/c sexp error?)
(define (sys cmd)
  (define op (open-output-string))
  (define ep (open-output-string))
  (parameterize ([current-input-port (open-input-string "")]
                 [current-error-port ep]
                 [current-output-port op])
    (cond
      [(system cmd)
       (read (open-input-string (get-output-string op)))]
      [else
       (get-output-string ep)])))


(build-answers-hash)

(for ([(student hash) (in-hash answers-hash)])
  (call-with-output-file (format "summary/~a.graph-test.txt" student)
    (λ (port)
      (for ([testcase+passed (in-list (sort (hash-map hash list) string<=? #:key car))])
        (define testcase (list-ref testcase+passed 0))
        (define passed (list-ref testcase+passed 1))
        (fprintf port "~s\n" (list testcase passed))))
    #:exists 'truncate))
