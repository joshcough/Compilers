(ns hw1 
  (:use clojure.test))

(defrecord StringLit [s])
(defrecord Concat [l r])
(defrecord RestAfter [l r])

(defn parse [sexp]
  (cond 
    (symbol? sexp) (StringLit. (str sexp))
    (list? sexp)
      (let [[left op right] sexp]
        (case op
          & (Concat. (parse left) (parse right))
          % (RestAfter. (parse left) (parse right))
          :else (throw (IllegalArgumentException. (str "bad list: " sexp))))) 
    :else (throw (IllegalArgumentException. (str "bad arg: " sexp)))))
 
(defmulti interpret class)
(defmethod interpret StringLit [s] (:s s))  
(defmethod interpret Concat [c] (str (interpret (:l c)) (interpret (:r c))))  
(defmethod interpret RestAfter [ra]
  (let [l (interpret (:l ra)) r (interpret (:r ra)) i (.indexOf l r)]
    (if (< i 0) (throw (Exception. (str r " not found in " l)))
      		(.substring l (+ i (.length r))))))

(defn testcode [l r]
  (with-test (defn f [x] l) (is (= (interpret (parse l)) r)))
)
(testcode 'hello "hello") 
(testcode '((hello & x) & world) "helloxworld")
(testcode '((hello % l) & (aworld % a)) "loworld")
(testcode '(((a & b) & (a & b)) & ((a & b) & (a & b))) "abababab")
(testcode '((((((abababab % a) % b) % a) % b) % a) % b) "ab")
(testcode '(((((((a & b) & (a & b)) & ((a & b) & (a & b))) % ab) % ab) % ab) % ab) "")
(testcode '(filename.scm % .) "scm")

(run-tests)
