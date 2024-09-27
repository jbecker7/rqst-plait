#lang plait

;; =============================================================================
;; Interpreter: interpreter.rkt
;; =============================================================================

(require "support.rkt")

(define (eval [str : S-Exp]): Value
  (interp (parse str)))

;; DO NOT EDIT ABOVE THIS LINE =================================================

; (interp: (Expr -> Value))

(define (interp [expr : Expr] [env : Env]) : Value
  (type-case Expr expr
    ;; Handle constants (numbers, strings, booleans)
    [(e-num value) (v-num value)]
    [(e-str value) (v-str value)]
    [(e-bool value) (v-bool value)]

    ;; Handle binary operators (e-op)
    [(e-op op left right)
     (let ([left-val (interp left env)]
           [right-val (interp right env)])
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
     (let ([cond-val (interp condition env)])
       (type-case Value cond-val
         [(v-bool b)
          (if b
              (interp consq env)
              (interp altern env))]
         [else
          (error 'interp-error "Condition must evaluate to a boolean")]))]

    ;; Handle variables (e-var)
    ;; (var <name>)
    [(e-var name)
     (let ([val (hash-ref env name (lambda () (error 'interp-error (format "Unbound variable: ~a" name))))])
       val)]

    ;; Handle lambda expressions (e-lam)
    ;; (lam <var> <expr>)
    [(e-lam param body)
     (v-fun param body env)] ;; Capture the environment when creating the lambda

    ;; Handle function application (e-app)
    ;; (<expr> <expr>)
    [(e-app func-expr arg-expr)
     (let ([func-val (interp func-expr env)]) ;; Evaluate the function expression
       (type-case Value func-val
         [(v-fun param body closure-env)
          (let ([arg-val (interp arg-expr env)]) ;; Evaluate the argument
            (let ([new-env (hash-set closure-env param arg-val)]) ;; Extend the environment
              (interp body new-env)))] ;; Evaluate the body in the new environment
         [else (error 'interp-error "Expected function in application")]))]))
