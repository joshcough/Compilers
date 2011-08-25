(ns hw5
  (:use clojure.test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Num [n])
(defrecord Add [l r])
(defrecord Sub [l r])
(defrecord Id [i])
(defrecord If0 [test then else])
(defrecord Fun [arg body])
(defrecord App [fun arg])

(defrecord NumV [n])
(defrecord ClosureV (param body env))

(defn error [message] (throw (IllegalArgumentException. message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse : sexpr -> FnWAE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parse [sexpr]
  ;; {fun {a b} {+ a b}} => {fun {a} {fun {b} {+ a b}}}
  (defn parse-fun [formals body]
    (condp = (count formals)
      0 (error "bad syntax")
      1 (Fun. (first formals) (parse body))
      (Fun. (first formals) (parse-fun (rest formals) body))
    )
  )
  ;; parse-app : sexpr -> app
  ;; {FAE1 FAE2 FAE3 FAE4} => {app {app {app 1 2} 3} 4}
  (defn parse-app [sexpr]
    (defn helper [sexpr]
      (condp = (count sexpr)
        1 (parse (first sexpr))
        (App. (helper (drop-last sexpr)) (parse (last sexpr)))
      )
    )
    ;; if the list is one long...Fail
    (if (= 1 (count sexpr))
      (error "appliction without arguments")
      (helper sexpr)))
  (cond
    (number? sexpr) (Num. sexpr)
    (symbol? sexpr) (Id. sexpr)
    (list? sexpr)
     (condp = (first sexpr)
       '+ (let [[a l r] sexpr] (Add. (parse l) (parse r)))
       '- (let [[s l r] sexpr] (Sub. (parse l) (parse r)))
       ;; {with {x FAE1} FAE2} => {{fun {x} FAE2} FAE1}
       'with (let [[with [x e1] e2] sexpr] (App. (Fun. x (parse e2)) (parse e1)))
       'if0 (let [[i t th el] sexpr] (If0. (parse t) (parse th) (parse el)))
       'fun (parse-fun (second sexpr) (nth sexpr 2))
       (parse-app sexpr)
     )
    :else (error "unexpected token")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest parser-tests
  ;; parse basics
  (is (= (parse 7) (Num. 7)))
  (is (= (parse 'a) (Id. 'a)))
  (is (= (parse '(+ 6 7)) (Add. (Num. 6) (Num. 7))))
  (is (= (parse '(- 6 7)) (Sub. (Num. 6) (Num. 7))))
  (is (= (parse '(+ 6 (+ 6 7))) (Add. (Num. 6) (Add. (Num. 6) (Num. 7)))))
  (is (= (parse '(- 6 (- 6 7))) (Sub. (Num. 6) (Sub. (Num. 6) (Num. 7)))))
  (is (= (parse '(with (x 7) x)) (App. (Fun. 'x (Id. 'x)) (Num. 7))))
  (is (= (parse '(if0 0 1 2)) (If0. (Num. 0) (Num. 1) (Num. 2))))

  ;; parse application tests
  ;;(test/exn (parse-App. '(x)) "App.liction without arguments")
  (is (= (parse '(x y)) (App. (Id. 'x)(Id. 'y))))
  (is (= (parse '(x y z)) (App. (App. (Id. 'x)(Id. 'y)) (Id. 'z))))
  (is (= (parse '(w x y z)) (App. (App. (App. (Id. 'w)(Id. 'x)) (Id. 'y)) (Id. 'z))))
  (is (= (parse '(0 Q (+ 3 6))) (App. (App. (Num. 0) (Id. 'Q)) (Add. (Num. 3) (Num. 6)))))

  ;; parse fun tests
  ;;(test/exn (parse '(Fun. () x)) "bad syntax")
  (is (= (parse '(fun (x) x)) (Fun. 'x (Id. 'x))))
  (is (= (parse '(fun (x y) x)) (Fun. 'x (Fun. 'y (Id. 'x)))))
  (is (= (parse '(fun (x x) x)) (Fun. 'x (Fun. 'x (Id. 'x)))))
  (is (= (parse '(fun (x y z) (+ (+ x y) z)))
      (Fun. 'x (Fun. 'y (Fun. 'z (Add. (Add. (Id. 'x) (Id. 'y))(Id. 'z)))))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; utility functions for interp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn findl [pred l]
  (let [[x & xs] l] 
    (if (= nil x) nil (if (pred x) x (recur pred xs)))))

;; lookup: sym deferred-subs -> FAE-Val
(defn lookup [name env]
  (let [f (findl #(= (first %) name) env)]
    (if (= f nil) (error (str "free id:" name)) (second f))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interp : FAE deferred-subs -> FAE-Val
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti interpret (fn [node env] 
  (class node)))
(defmethod interpret Num [n env] (NumV. (:n n)))
(defmethod interpret Add [a env]
  (NumV. (+ (:n (interpret (:l a) env)) (:n (interpret (:r a) env)))))
(defmethod interpret Sub [a env]
  (NumV. (- (:n (interpret (:l a) env)) (:n (interpret (:r a) env)))))
(defmethod interpret Id  [i env] (lookup (:i i) env))
(defmethod interpret If0 [i env] 
  (let [t (interpret (:test i) env)]
    (interpret (if (= t (NumV. 0)) (:then i) (:else i)) env)))
(defmethod interpret Fun [fun env] (ClosureV. (:arg fun) (:body fun) env))
(defmethod interpret App [app env]
  (let [a (interpret (:arg app) env)
        f (interpret (:fun app) env)]
             ;;;[numV (n) (error "application expected procedure")]
    (interpret (:body f) (cons (list (:param f) a) (:env f))) 
  )
)

(defn interp [node] 
  (let [result (interpret (parse node) '())]
    (if (instance? NumV result) (:n result) 'procedure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interpreter tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest interpreter-tests
  ;; interpret basics
  (is (= (interp 7) 7))
  (is (= (interp '(- 10 4)) 6))
  (is (= (interp '(+ 3 5)) 8))
  (is (= (interpret (parse `x) `((x ~(NumV. 10)))) (NumV. 10)))
  (is (= (interp '(fun (x) x)) 'procedure))
  (is (= (interp '((fun (x) (+ x 2)) 5)) 7))
  (is (= (interp '((fun (x) (fun (y) (+ x y))) 5 2)) 7))
  (is (= (interp '((fun (x) (fun (y) (+ x y))) 5)) 'procedure))
  (is (= (interp '(fun (x) (+ x 2))) 'procedure))
  (is (= (interp '(if0 0 1 2)) 1))
  (is (= (interp '(if0 1 1 2)) 2))

  ;; bad math test
  ;(test/exn (interp-expr (parse '{+ {fun {x} x} 1}))
  ;        "numeric operation expected number")

  ;; bad application test
  ;(test/exn (interp-expr (parse '{1 2})) "application expected procedure")
  ;(test/exn (interp-expr (parse '(0 f A 2 Y 0 w A))) "free identifier")
  ;(test/exn (interp-expr (parse '(0 Q (+ 3 6)))) "free identifier")


  ;(is (= (interp '((fun (x) (fun (x) x)) 5 6))) 6)

  ;; test for functions with the same id in arg list more than once
  (is (= (interp '((fun (x x) x) 5 6))) 6)
  (is (= (interp '((fun (x x x) x) 5 6 7))) 7)
  (is (= (interp '((fun (x y x) x) 5 6 7))) 7)
  (is (= (interp '((fun (x x y) x) 5 6 7))) 6)
  (is (= (interp '((fun (x x y y) y) 5 6 7 8))) 8)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; primitive functions
(def pair '(fun (l r) (fun (b) (b l r))))
(def fst  '(fun (p) (with (true (fun (x y) x)) (p true))))
(def snd  '(fun (p) (with (false (fun (x y) y)) (p false))))

; Y combinator
(def Y
  `(fun (X)
    ((fun (p) (X (fun (arg) ((p p) arg))))
     (fun (p) (X (fun (arg) ((p p) arg)))))))

;; guy steele's defs from: 
;; http://groups.csail.mit.edu/mac/users/gjs/6.945/readings/MITApril2009Steele.pdf
;(defn (mapreduce f g id xs)
;  (cond ((null? xs) id)
;        (else (g (f (car xs)) (mapreduce f g id (cdr xs))))))
;(defn (map f xs) (mapreduce (λ (x) (list (f x))) append '() xs))
;(defn (reduce g id xs) (mapreduce (λ (x) x) g id xs))

(def mapreduce 
  `(with (Y ~Y) 
         (Y (fun (mr) 
                 (fun (f g id xs) 
                      (if0 xs id 
                           (g (f (fst xs)) (mr f g id (snd xs)))))))))

(def reduce `(with (mapreduce ~mapreduce) 
                      (fun (g id xs) (mapreduce (fun (x) x) g id xs))))  

(def addf '(fun (x y) (+ x y)))

;; sum definition for the homework
(def sum `(with (reduce ~reduce) (fun (l) (reduce ~addf 0 l))))

(defn wrap-with-hw-lib [expr]
  `(with (pair ~pair) 
         (with (fst ~fst) 
               (with (snd ~snd) 
                     (with (sum ~sum) ~expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pair tests (require hw lib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn hw-interp [sexp] 
  (println (wrap-with-hw-lib sexp))
  (interp (wrap-with-hw-lib sexp))
)

(deftest hw-tests
  ;; interpret basics
  (is (= (hw-interp '(fst (pair 2 3))) 2))
)

(run-tests)

(test (hw-interp '(fst (pair 2 3))) 2)
(test (hw-interp '(snd (pair 2 0))) 0)
(test (hw-interp '(snd (pair 1 2))) 2)
(test (hw-interp '(snd (fst (pair (pair 1 2) (pair 3 4))))) 2)
(test (hw-interp '(with (p (pair 1 2))(+ (fst p) (snd p)))) 3)
(test (hw-interp '(with (p (pair (fun (x) (+ x 1)) 2))((fst p) (snd p)))) 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sum tests (require hw lib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (hw-interp '(sum 0)) 0)
(test (hw-interp '(sum (pair 1 0))) 1)
(test (hw-interp '(sum (pair 2 (pair 1 0)))) 3)
(test (hw-interp '(sum (pair 9 (pair 2 (pair 1 0))))) 12)
(test (hw-interp '(sum (pair 0 (pair 9 (pair 2 (pair 1 0)))))) 12)
(test (hw-interp '(sum (pair 0 (pair 0 (pair 0 (pair 0 0)))))) 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scope games using hw lib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (hw-interp '(with (sum 5) sum)) 5)
(test (hw-interp '((with (sum (fun (x) (+ x 5))) sum) 7)) 12)
(test (hw-interp 
       '(with (sum (fun (l) (+ 5 (sum l)))) 
              (sum (pair 0 (pair 0 (pair 0 (pair 0 0))))))) 
      5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEYOND HOMEWORK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; everything below this point is above what was required for the homework
;; and therefore you need not read it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some important comments about this...
;;
;; this line was taken from the homework:
;; (with (pair ...) (with (fst ...) (with (snd ...) (with (sum ...) (sum 0)))))
;;
;; always wrapping an expression in the above definitions means that those
;; definitions are 'the library' since the homework does this automatically, 
;; its not easy to add more functions to the library. 
;;
;; to satisfy the requirements for the homework, sum explicitely includes 
;; addf, and reduce (which in turn includes mapreduce, which includes Y).
;; 
;; i would like to have not done that, but instead, would have liked to
;; add those functions to the library directly.
;;
;; by adding them to the library, sum could call reduce and addf
;; without explicitely including it in a with statement, like so:
;; (defn sum  `(fun (l) (reduce addf 0 l)))
;; 
;; sum is allowed to call pair, fst, snd implicitely because the HW includes 
;; them in the library
;;
;;
;; below is my attempt at creating that library.
;; these functions get put into the ds repo in fae.
;; each closure has a reference to the previous ds repo, 
;; so that each function has a reference to the functions
;; created before it. 
;;
;; it very clear that a particular function
;; DOESNT depend on a function declared before it. 
;;
(defn (create-lib depends-on-libs pairs)
  (defn (create-lib-func sym body ds)
    (cons (symValPair sym (interp (parse body) ds)) ds))
  (foldl
   (lambda (pair acc-ds)
     (create-lib-func (first pair) (second pair) acc-ds))
   (foldl append '() depends-on-libs)
   pairs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn base-lib 
  (create-lib '()
   (list (list 'Y Y)
         (list 'pair pair)
         (list 'fst fst)
         (list 'snd snd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boolean library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn boolean-lib
  (create-lib (list base-lib)
   (list
    (list 'or '(fun (x y) (if0 x 0 (if0 y 0 1))))
    (list 'and '(fun (x y) (if0 x (if0 y 0 1) 1)))
    (list 'zero? '(fun (x) (if0 x 0 1)))
    (list 'not '(fun (x) (if0 x 1 0)))
    )))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; math library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn math-lib
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

(defn list-lib
  (create-lib (list math-lib base-lib)
   (list 
    (list 'mapreduce  
          `(Y (fun (MR) 
                   (fun (f g id xs) 
                        (if0 xs id (g (f (fst xs)) (MR f g id (snd xs))))))))
    (list 'reduce `(fun (g id xs) (mapreduce (fun (x) x) g id xs)))
    (list 'map `(fun (f xs) (mapreduce f (fun (x y) (pair x y)) 0 xs)))
    (list 'sum `(fun (l) (reduce addf 0 l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; finally, the entire fae library.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fae-lib (create-lib (list list-lib math-lib boolean-lib base-lib) '()))

;; parse the expression, and then interpret (with access to the main library)
(defn (fae sexpr)
  (type-case FAE-Val (interp (parse sexpr) fae-lib)
             [numV (n) n]
             [closureV (s b ds) 'procedure]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (fae '(+ 5 6)) 11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pair tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (fae '(fst (pair 2 3))) 2)
(test (fae '(snd (pair 2 0))) 0)
(test (fae '(snd (pair 1 2))) 2)
(test (fae '(snd (fst (pair (pair 1 2) (pair 3 4))))) 2)
(test (fae '(with (p (pair 1 2))(+ (fst p) (snd p)))) 3)
(test (fae '(with (p (pair (fun (x) (+ x 1)) 2))((fst p) (snd p)))) 3)

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
