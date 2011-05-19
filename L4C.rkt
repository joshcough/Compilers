#lang plai

(define (main . argv)
  (match argv
    [(list path)
     (call-with-input-file path
       (compose pretty-write normalize-prog read))]))

(define (normalize-prog p)
  (define (normalize-expr e)
    (find e (no-ctxt)))
  (match (freshen p)
    [`(,main (,ls ,xss ,es) ...)
     (cons (normalize-expr main)
           (map (λ (l xs e) (list l xs (normalize-expr e)))
                ls xss es))]))

(define (val? v) (or (symbol? v) (number? v)))
(define (L4-e? e) #t)
(define count (box 0))
(define (maybe-let d f) (if (val? d) (f d) (let ([x (fresh-var)]) `(let ([,x ,d]) ,(f x)))))

(define-type context
  [let-ctxt (x symbol?) (b L4-e?) (k context?)]
  [if-ctxt (t L4-e?) (e L4-e?) (k context?)]
  [en-ctxt (es (listof L4-e?)) (vs (listof val?)) (k context?)]
  [no-ctxt])

; find: L4-e context -> L3-e
(define (find e k)
  (match e
    [`(let ([,x ,r]) ,b) (find r (let-ctxt x b k))]
    [`(if ,c ,t ,e) (find c (if-ctxt t e k))]
    [`(begin ,e1 ,e2) (find `(let ([,(fresh-var) ,e1]) ,e2) k)]
    [`(,e ,es ...) (find e (en-ctxt es `() k))]
    [(? val?) (fill e k)]))

; fill: L3-d context -> L3-e
(define (fill d k)
  (type-case context k
    [let-ctxt (x b k) `(let ([,x ,d]),(find b k))]
    [if-ctxt (t e k) (maybe-let d (λ (v) `(if ,v ,(find t k) ,(find e k))))]
    [en-ctxt (es vs k)
      (maybe-let d 
        (λ (v) (define vs* (append vs (list v)))
          (match es
            ['() (fill vs* k)]
            [(cons e es*) (find e (en-ctxt es* vs* k))])))]
    [no-ctxt () d]))

(define (freshen e)
  (define renamings (make-hash))
  (define (rename x)
    (hash-ref renamings x
              (λ ()
                (define y (fresh-var))
                (hash-set! renamings x y)
                y)))
  (let recur ([e e])
    (match e
      [(? symbol?)
       (if (variable? e)
           (rename e)
           e)]
      [(? list?)
       (map recur e)]
      [_ e])))

(define (fresh-var) 
  (string->symbol
   (string-append "x" 
                  (number->string
                   (begin
                     (set-box! count (+ 1 (unbox count)))
                     (unbox count))))))

(define (variable? x)
  (and (symbol? x)
       (not (member x
                    '(let
                         if
                       new-array
                       new-tuple
                       aref
                       aset
                       alen
                       begin
                       print
                       make-closure
                       closure-proc
                       closure-vars
                       + - * < <= =
                       number? a?)))
       (not (regexp-match? #rx":" (symbol->string x)))))