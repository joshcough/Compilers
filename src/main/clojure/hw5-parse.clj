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
