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


;; Additional Tests for Edge Cases and Chaff Detection (I missed a few)

;; Test adding a boolean to a number (should raise an error)
(test-raises-error? "Fails on addition of boolean and number"
  (eval `(+ true 5)))

;; Test string equality with mixed types
(test-raises-error? "Fails on string equality with number"
  (eval `(str= "string" 5)))

;; Test string concatenation with a boolean (should raise an error)
(test-raises-error? "Fails on invalid string concatenation with boolean"
  (eval `(++ true "string")))

;; Test number equality with a boolean (should raise an error)
(test-raises-error? "Fails on number equality with boolean"
  (eval `(num= 5 true)))


;; Test if with number as condition (should raise an error)
(test-raises-error? "Fails on if with number as condition"
  (eval `(if 1 "yes" "no")))

;; Test nested if with invalid condition (non-boolean condition)
(test-raises-error? "Fails on nested if with invalid condition"
  (eval `(if true (if 5 "yes" "no") "no")))


;; Test applying a non-function as a function (e.g., a string)
(test-raises-error? "Fails on applying string as function"
  (eval `("string" 2)))

;; Test applying a boolean as a function
(test-raises-error? "Fails on applying boolean as function"
  (eval `(true 1)))

;; Test lambda returning another lambda and applying the result
(test-equal? "Works with lambda returning another lambda"
  (eval `(((lam x (lam y (+ x y))) 3) 4)) (v-num 7))

;; Test applying a lambda with incorrect argument types (e.g., string instead of number)
(test-raises-error? "Fails on applying lambda with incorrect argument type"
  (eval `((lam x (+ x 3)) "not a number")))


;; Test unbound variable usage (should raise an error)
(test-raises-error? "Fails on using unbound variable"
  (eval `(let1 y 5 x)))

;; Test variable defined inside lambda (should raise an error when used outside)
(test-raises-error? "Fails on using variable outside of lambda"
  (eval `((lam x (+ x y)) 3)))

;; Test for handling empty string in string concatenation
(test-equal? "Works with empty string concatenation"
  (eval `(++ "" "test")) (v-str "test"))

;; Test for handling zero in addition
(test-equal? "Works with zero in addition"
  (eval `(+ 0 5)) (v-num 5))

;; Test for variable binding using a lambda expression
(test-equal? "Lambda binds variable and performs addition"
  (eval `((lam x (+ x 3)) 5))
  (v-num 8))

;; Test for variable shadowing within nested lambdas
(test-equal? "Variable shadowing in nested lambdas"
  (eval `(((lam x (lam x (+ x 3))) 5) 10))
  (v-num 13))


;; DO NOT EDIT BELOW THIS LINE =================================================

(module+ main
  (run-tests sample-tests)
  (run-tests student-tests))