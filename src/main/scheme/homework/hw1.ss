;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(planet plai/plai:1:6/lang/reader)

(define-type EXP
  [string-lit (n string?)]
  [concat (left EXP?) (right EXP?)]
  [rest-after (left EXP?) (right EXP?)]
)

(define (parse sexp)
  (cond
    [(string? sexp) (string-lit sexp)]
    [(list? sexp)
     (case (second sexp)
       [(&) (concat (parse (first sexp)) (parse (third sexp)))]
       [(@) (rest-after (parse (first sexp)) (parse (third sexp)))]
       )]
    [else (error "unexpected token")]
    ))
       
(define (crazy-string-find-thing findme instring)
  (cond
    [(> (string-length findme) (string-length instring)) (error (string-append findme " not found in " instring))]
    [(string=? findme (substring instring 0 (string-length findme))) (substring instring (string-length findme))]
    [else (crazy-string-find-thing findme (substring instring 1))]))

(define (interpret exp)
  (type-case EXP exp
             [string-lit (s) s]
             [concat (l r) (string-append (interpret l) (interpret r))]
             [rest-after (l r) (crazy-string-find-thing (interpret r) (interpret l))]))

(define (testcode l r)(test (interpret(parse l)) r))
(define (testcode/exn l r)(test/exn (interpret(parse l)) r))

(testcode "" "")
(testcode "hello" "hello")
(testcode '(("hello" & " ") & "world") "hello world")
(testcode '(("hello" @ "l") & ("a world" @ "a")) "lo world")
(testcode '((("a" & "b") & ("a" & "b")) & (("a" & "b") & ("a" & "b"))) "abababab")
(testcode '(((((("abababab" @ "a") @ "b") @ "a") @ "b") @ "a") @ "b") "ab")
(testcode '((((((("a" & "b") & ("a" & "b")) & (("a" & "b") & ("a" & "b"))) @ "ab") @ "ab") @ "ab") @ "ab") "")
(testcode '("filename.scm" @ ".") "scm")
(testcode/exn 6 "unexpected token")
(testcode/exn '("x" @ "hello") "hello not found in x")
(testcode/exn '("" @ "x") "x not found in ")
(testcode/exn '(("" & "") @ "x") "x not found in ")
(testcode/exn '(((((((("a" & "b") & ("a" & "b")) & (("a" & "b") & ("a" & "b"))) @ "ab") @ "ab") @ "ab") @ "ab") @ "x") "x not found in ")