#lang racket

;; =============================================================================
;; Interpreter: interpreter-tests.rkt
;; =============================================================================

(require (only-in "interpreter.rkt" eval)
         "support.rkt"
         "test-support.rkt")

; Here, we provide some examples of how to use the testing forms provided in
; "test-support.rkt". You should not use any external testing library other
; than those specifically provided; otherwise, we will not be able to grade
; your code.
(define/provide-test-suite sample-tests
  ;; DO NOT ADD TESTS HERE
  (test-equal? "Works with Num primitive"
               (eval `2) (v-num 2))
  (test-raises-error? "Passing Str to + results in error"
                             (eval `{+ "bad" 1}))
  (test-pred "Equivalent to the test case above, but with test-pred"
             v-fun? (eval `{lam x 5})))

;; DO NOT EDIT ABOVE THIS LINE =================================================

(define/provide-test-suite student-tests ;; DO NOT EDIT THIS LINE ==========

;; Constants
  
;; Test for string constants
(test-equal? "Works with strings"
  (eval `"Hello World!") (v-str "Hello World!"))

;; Test for boolean constants
(test-equal? "Works with booleans"
  (eval `true) (v-bool #t))

(test-equal? "Works with booleans - false"
  (eval `false) (v-bool #f))

;; Test for number constants
(test-equal? "Works with numbers"
  (eval `5) (v-num 5))

;; Binary Operators

 ;; Test for addition (op-plus)
(test-equal? "Works with addition"
  (eval `(+ 3 7)) (v-num 10))

;; Test for string concatenation (op-append)
(test-equal? "Works with string concatenation"
  (eval `(++ "Hello, " "World!")) (v-str "Hello, World!"))

;; Test for number equality (op-num-eq)
(test-equal? "Works with number equality - true"
  (eval `(num= 10 10)) (v-bool #t))

(test-equal? "Works with number equality - false"
  (eval `(num= 5 10)) (v-bool #f))

;; Test for string equality (op-str-eq)
(test-equal? "Works with string equality - true"
  (eval `(str= "abc" "abc")) (v-bool #t))

(test-equal? "Works with string equality - false"
  (eval `(str= "abc" "xyz")) (v-bool #f))


;; Tests for conditionals
  
;; Test for conditionals where condition is true
(test-equal? "Works with if - true condition"
  (eval `(if true "yes" "no")) (v-str "yes"))

;; Test for conditionals where condition is false
(test-equal? "Works with if - false condition"
  (eval `(if false "yes" "no")) (v-str "no"))


;; Tests for variables



;; Tests for lambdas and functions


;; Test for simple lambda application
(test-equal? "Works with lambda application"
  (eval `((lam x (+ x 3)) 2)) (v-num 5))

;; Test for constant function (ignores the argument)
(test-equal? "Works with constant function"
  (eval `((lam y 5) 1)) (v-num 5))

;; Test for nested lambdas (function that returns a function)
(test-equal? "Works with nested lambdas"
  (eval `(((lam x (lam y (+ x y))) 5) 7)) (v-num 12))


  
;; Tests for error logging

;; Error: trying to add a number and a string
(test-raises-error? "Fails on invalid addition"
  (eval `(+ 5 "not a number")))

;; Error: trying to concatenate a number with a string
(test-raises-error? "Fails on invalid concatenation"
  (eval `(++ 5 "string")))

;; Error: invalid type for if condition (should be a boolean)
(test-raises-error? "Fails on non-boolean condition in if"
  (eval `(if 5 "yes" "no")))
)

;; Error: applying a non-function (e.g., trying to call a number)
(test-raises-error? "Fails on applying a non-function"
  (eval `(5 3)))

;; Error: using an unbound variable
(test-raises-error? "Fails on unbound variable"
  (eval `x))

;; Error: using an unbound variable inside a lambda
(test-raises-error? "Fails on unbound variable inside lambda"
  (eval `((lam x (+ x y)) 3)))

;; DO NOT EDIT BELOW THIS LINE =================================================

(module+ main
  (run-tests sample-tests)
  (run-tests student-tests))