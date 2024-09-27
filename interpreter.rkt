#lang plait

;; =============================================================================
;; Interpreter: interpreter.rkt
;; =============================================================================

(require "support.rkt")

(define (eval [str : S-Exp]): Value
  (interp (parse str)))

;; DO NOT EDIT ABOVE THIS LINE =================================================

; (interp: (Expr -> Value))

(define initial-env (hash empty))  ;; Define the initial environment as an empty hash

;; Wrapper around interp that provides a default environment if none is supplied (since we can't modify line 9)
(define (interp [expr : Expr]) : Value
  (interp-with-env expr initial-env))  ;; Call helper with the empty environment to start

;; The actual interpreter function that requires both the expression and environment
(define (interp-with-env [expr : Expr] [env : Env]) : Value
  (type-case Expr expr
    ;; Handle constants (numbers, strings, booleans)
    [(e-num value) (v-num value)]
    [(e-str value) (v-str value)]
    [(e-bool value) (v-bool value)]

    ;; Handle binary operators (e-op)
    [(e-op op left right)
     (let ([left-val (interp-with-env left env)]
           [right-val (interp-with-env right env)])
       ;; Separate subcategories of operators
       (type-case Operator op
         ;; (+ <expr> <expr>)
         [(op-plus)
          (type-case Value left-val
            [(v-num l)
             (type-case Value right-val
               [(v-num r) (v-num (+ l r))]
               [else (error 'interp-error "Expected number on RHS of +")])]
            [else (error 'interp-error "Expected number on LHS of +")])]
         ;; (++ <expr> <expr>)
         [(op-append)
          (type-case Value left-val
            [(v-str l)
             (type-case Value right-val
               [(v-str r) (v-str (string-append l r))]
               [else (error 'interp-error "Expected string on RHS of ++")])]
            [else (error 'interp-error "Expected string on LHS of ++")])]
         ;; (num= <expr> <expr>)
         [(op-num-eq)
          (type-case Value left-val
            [(v-num l)
             (type-case Value right-val
               [(v-num r) (v-bool (= l r))]
               [else (error 'interp-error "Expected number on RHS of num=")])]
            [else (error 'interp-error "Expected number on LHS of num=")])]
         ;; (str= <expr> <expr>)
         [(op-str-eq)
          (type-case Value left-val
            [(v-str l)
             (type-case Value right-val
               [(v-str r) (v-bool (string=? l r))]
               [else (error 'interp-error "Expected string on RHS of str=")])]
            [else (error 'interp-error "Expected string on LHS of str=")])]))]

    ;; Handle conditionals (e-if)
    ;; (if <expr> <expr> <expr>)
    [(e-if condition consq altern)
     (let ([cond-val (interp-with-env condition env)])
       (type-case Value cond-val
         [(v-bool b)
          (if b
              (interp-with-env consq env)
              (interp-with-env altern env))]
         [else
          (error 'interp-error "Condition must evaluate to a boolean")]))]

    ;; Handle variables (e-var)
    ;; (var <name>)
    [(e-var name)
     (lookup name env)]  ;; Use lookup helper function for variable resolution

    ;; Handle lambda expressions (e-lam)
    ;; (lam <var> <expr>)
    [(e-lam param body)
     (v-fun param body env)] ;; Capture the environment when creating the lambda

    ;; Handle function application (e-app)
    ;; (<expr> <expr>)
    [(e-app func-expr arg-expr)
     (let ([func-val (interp-with-env func-expr env)]) ;; Evaluate the function expression
       (type-case Value func-val
         [(v-fun param body closure-env)
          (let ([arg-val (interp-with-env arg-expr env)]) ;; Evaluate the argument
            (let ([new-env (extend closure-env param arg-val)]) ;; Extend the environment
              (interp-with-env body new-env)))] ;; Evaluate the body in the new environment
         [else (error 'interp-error "Expected function in application")]))]))

;; Helper functions for environment management

;; Lookup a variable in the environment
(define (lookup [s : Symbol] [n : Env]) : Value
  (type-case (Optionof Value) (hash-ref n s)
    [(none) (error 'interp-error (string-append "Unbound variable: " (symbol->string s)))]
    [(some v) v]))

;; Extend the environment by adding a new variable binding
(define (extend [old-env : Env] [new-name : Symbol] [value : Value]) : Env
  (hash-set old-env new-name value))
