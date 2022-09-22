#lang racket

(provide eval-exp
         init-env)

(require "env.rkt"
         "parse.rkt")

(define primitive-operators '(+ - * / add1 sub1 negate list cons car cdr
                                eqv? lt? gt? leq? geq? null? list? number?))

(define prim-env
  (env primitive-operators
       (map box (map prim-proc primitive-operators))
       empty-env))

(define init-env (env '(x y null True False) (map box '(23 42 () True False)) prim-env))

(struct closure (params body env) #:transparent)

(define (eval-exp tree e)
  (cond [(lit-exp? tree) (lit-exp-num tree)]
        [(var-exp? tree) (unbox (env-lookup e (var-exp-symbol tree)))]
        [(app-exp? tree)
         (let ([proc (eval-exp (app-exp-proc tree) e)]
               [args (map (λ (arg)
                            (eval-exp arg e))
                          (app-exp-args tree))])
           (apply-proc proc args))]
        [(ite-exp? tree) (let ([cond (eval-exp (ite-exp-cond tree) e)])
                           (if (or (eqv? cond 0) (eqv? cond 'False))
                               (eval-exp (ite-exp-else tree) e)
                               (eval-exp (ite-exp-then tree) e)))]
        [(let-exp? tree) (eval-exp (let-exp-body tree)
                                   (env (let-exp-syms tree)
                                        (map box (map (λ (exp)
                                                        (eval-exp exp e))
                                                      (let-exp-exps tree)))
                                        e))]
        [(lambda-exp? tree) (closure (lambda-exp-params tree)
                                     (lambda-exp-body tree)
                                     e)]
        [(set-exp? tree) (set-box! (env-lookup e (set-exp-sym tree))
                                   (eval-exp (set-exp-exp tree) e))]
        [(begin-exp? tree) (foldl (λ (exp acc) (eval-exp exp e))
                                  (void)
                                  (begin-exp-exps tree))]
        [else (error 'eval-exp "Invalid tree: ~s" tree)]))

(define (apply-proc proc args)
  (cond [(prim-proc? proc)
         (apply-primitive-op (prim-proc-op proc) args)]
        [(closure? proc) (eval-exp (closure-body proc)
                                   (env (closure-params proc) (map box args) (closure-env proc)))]
        [else (error 'apply-proc "bad procedure: ~s" proc)]))

(define (apply-primitive-op op args)
  (cond [(eq? op '+) (apply + args)]
        [(eq? op '-) (apply - args)]
        [(eq? op '*) (apply * args)]
        [(eq? op '/) (apply / args)]
        [(eq? op 'add1) (apply add1 args)]
        [(eq? op 'sub1) (apply sub1 args)]
        [(eq? op 'negate) (apply (λ (num) (- 0 num)) args)]
        [(eq? op 'list) (apply list args)]
        [(eq? op 'cons) (apply cons args)]
        [(eq? op 'car) (apply car args)]
        [(eq? op 'cdr) (apply cdr args)]
        [(eq? op 'eqv?) (if (apply eqv? args) (eval-exp (var-exp 'True) init-env) (eval-exp (var-exp 'False) init-env))]
        [(eq? op 'lt?) (if (apply < args) (eval-exp (var-exp 'True) init-env) (eval-exp (var-exp 'False) init-env))]
        [(eq? op 'gt?) (if (apply > args) (eval-exp (var-exp 'True) init-env) (eval-exp (var-exp 'False) init-env))]
        [(eq? op 'leq?) (if (apply <= args) (eval-exp (var-exp 'True) init-env) (eval-exp (var-exp 'False) init-env))]
        [(eq? op 'geq?) (if (apply >= args) (eval-exp (var-exp 'True) init-env) (eval-exp (var-exp 'False) init-env))]
        [(eq? op 'null?) (if (apply null? args) (eval-exp (var-exp 'True) init-env) (eval-exp (var-exp 'False) init-env))]
        [(eq? op 'list?) (if (apply list? args) (eval-exp (var-exp 'True) init-env) (eval-exp (var-exp 'False) init-env))]
        [(eq? op 'number?) (if (apply number? args) (eval-exp (var-exp 'True) init-env) (eval-exp (var-exp 'False) init-env))]
        [else (error 'apply-primitive-op "Unknown primitive: ~s" op)]))
