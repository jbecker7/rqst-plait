#lang plait

;; =============================================================================
;; Interpreter: interpreter.rkt
;; =============================================================================

(require "support.rkt")

(define (eval [str : S-Exp]): Value
  (interp (parse str)))

;; DO NOT EDIT ABOVE THIS LINE =================================================

; (interp: (Expr -> Value))

(define (interp [expr : Expr]) : Value
  (type-case Expr expr
    ;; Handle constants (numbers, strings, booleans)
    [(e-num value) (v-num value)]
    [(e-str value) (v-str value)]
    [(e-bool value) (v-bool value)]

    ;; Handle binary operators (e-op)
    [(e-op op left right)
     (let ([left-val (interp left)]
           [right-val (interp right)])
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
     (let ([cond-val (interp condition)])
       (type-case Value cond-val
         [(v-bool b)
          (if b
              (interp consq)
              (interp altern))]
         [else
          (error 'interp-error "Condition must evaluate to a boolean")]))]))
