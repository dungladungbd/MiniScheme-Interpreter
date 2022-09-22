#lang racket

(require rackunit)
(require "env.rkt" "parse.rkt" "interp.rkt")

(provide interp-tests)

(define test-env
  (env '(foo bar) (map box '(10 23)) init-env))

(define interp-tests
  (test-suite
   "Interpreter tests"
   (test-eqv? "Literal"
              (eval-exp (lit-exp 5) empty-env)
              5)

   (test-eqv? "Variable x"
              (eval-exp (var-exp 'x) test-env)
              23)

   (test-eqv? "Variable foo"
              (eval-exp (var-exp 'foo) test-env)
              10)
   
   (test-eqv? "Primitive +"
              (eval-exp (app-exp (var-exp '+) (list (var-exp 'x) (var-exp 'foo))) test-env)
              33)

   (test-eqv? "Primitive -"
              (eval-exp (app-exp (var-exp '-) (list (var-exp 'x) (var-exp 'foo))) test-env)
              13)

   (test-eqv? "Primitive *"
              (eval-exp (app-exp (var-exp '*) (list (var-exp 'x) (var-exp 'foo))) test-env)
              230)

   (test-eqv? "Primitive /"
              (eval-exp (app-exp (var-exp '/) (list (var-exp 'x) (var-exp 'foo))) test-env)
              23/10)

   (test-eqv? "Primitive add1"
              (eval-exp (app-exp (var-exp 'add1) (list (var-exp 'x))) test-env)
              24)

   (test-eqv? "Primitive sub1"
              (eval-exp (app-exp (var-exp 'sub1) (list (var-exp 'x))) test-env)
              22)

   (test-eqv? "Primitive negate"
              (eval-exp (app-exp (var-exp 'negate) (list (var-exp 'x))) test-env)
              -23)

   (test-equal? "Primitive list"
                (eval-exp (app-exp (var-exp 'list) (list (var-exp 'x) (var-exp 'foo))) test-env)
                (list 23 10))

   (test-equal? "Primitive cons"
                (eval-exp (app-exp (var-exp 'cons) (list (var-exp 'x) (var-exp 'null))) test-env)
                (list 23))

   (test-equal? "Primitive car"
                (eval-exp (app-exp (var-exp 'car) (list (app-exp (var-exp 'list) (list (var-exp 'x) (var-exp 'foo))))) test-env)
                23)

   (test-equal? "Primitive cdr"
                (eval-exp (app-exp (var-exp 'cdr) (list (app-exp (var-exp 'list) (list (var-exp 'x) (var-exp 'foo))))) test-env)
                (list 10))

   (test-eqv? "Primitive eqv?"
              (eval-exp (app-exp (var-exp 'eqv?) (list (var-exp 'x) (lit-exp 23))) test-env)
              'True)

   (test-eqv? "Primitive lt?"
              (eval-exp (app-exp (var-exp 'lt?) (list (var-exp 'x) (lit-exp 232))) test-env)
              'True)

   (test-eqv? "Primitive gt?"
              (eval-exp (app-exp (var-exp 'gt?) (list (var-exp 'x) (lit-exp 232))) test-env)
              'False)

   (test-eqv? "Primitive leq?"
              (eval-exp (app-exp (var-exp 'leq?) (list (var-exp 'x) (lit-exp 232))) test-env)
              'True)

   (test-eqv? "Primitive geq?"
              (eval-exp (app-exp (var-exp 'geq?) (list (var-exp 'x) (lit-exp 232))) test-env)
              'False)

   (test-eqv? "Primitive null?"
              (eval-exp (app-exp (var-exp 'null?) (list (var-exp 'null))) test-env)
              'True)

   (test-eqv? "Primitive list?"
              (eval-exp (app-exp (var-exp 'list?) (list (app-exp (var-exp 'list) (list (var-exp 'x) (var-exp 'foo))))) test-env)
              'True)

   (test-eqv? "Primitive number?"
              (eval-exp (app-exp (var-exp 'number?) (list (var-exp 'x))) test-env)
              'True)
   
   (test-eqv? "If-then-else then"
              (eval-exp (ite-exp (var-exp 'foo) (lit-exp 5) (lit-exp 6)) test-env)
              5)

   (test-eqv? "If-then-else else"
              (eval-exp (ite-exp (var-exp 'False) (lit-exp 5) (lit-exp 6)) test-env)
              6)

   (test-eqv? "Let"
              (eval-exp (let-exp (list 'a 'b) (list (lit-exp 1) (lit-exp 5)) (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b)))) test-env)
              6)

   (test-eqv? "Let inside of let"
              (eval-exp (let-exp (list 'a 'b)
                                 (list (app-exp (var-exp '*) (list (lit-exp 2) (lit-exp 3))) (lit-exp 24))
                                 (let-exp (list 'c)
                                          (list (app-exp (var-exp '-) (list (var-exp 'b) (var-exp 'a))))
                                          (app-exp (var-exp '*) (list (var-exp 'c) (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b)))))))
                        test-env)
              540)

   (test-equal? "Lambda exp no proc"
                (eval-exp (app-exp (lambda-exp '(x) (var-exp 'x)) (list (lit-exp 1)))
                          test-env)
                1)

   (test-equal? "Lambda exp with proc"
                (eval-exp (app-exp
                           (lambda-exp '(x y) (app-exp (var-exp '*) (list (var-exp 'x) (var-exp 'y))))
                           (list (lit-exp 2) (lit-exp 4)))
                          test-env)
                8)

   (test-equal? "Lambda exp inside let"
                (eval-exp (let-exp
                           '(sqr)
                           (list (lambda-exp '(x) (app-exp (var-exp '*) (list (var-exp 'x) (var-exp 'x)))))
                           (app-exp (var-exp 'sqr) (list (lit-exp 64))))
                          test-env)
                4096)

   (test-equal? "Lambda inside let inside lambda inside let"
                (eval-exp (let-exp
                           '(sqr)
                           (list (lambda-exp '(x)
                                             (app-exp (var-exp '*) (list (var-exp 'x) (var-exp 'x)))))
                           (let-exp
                            '(cube)
                            (list
                             (lambda-exp '(x)
                                         (app-exp (var-exp '*)
                                                  (list (var-exp 'x) (app-exp (var-exp 'sqr) (list (var-exp 'x)))))))
                            (app-exp (var-exp 'cube) (list (lit-exp 3)))))
                          test-env)
                27)

   (test-equal? "Begin + set"
                (eval-exp (begin-exp (list (set-exp 'bar (lit-exp 2))
                                           (app-exp (var-exp '+) (list (var-exp 'bar) (var-exp 'y)))))
                          test-env)
                44)

   (test-equal? "Begin + set inside let"
                (eval-exp (let-exp
                           '(k y)
                           (list (lit-exp 1) (lit-exp 2))
                           (begin-exp (list (set-exp 'k (lit-exp 5))
                                            (app-exp (var-exp '+) (list (var-exp 'k) (var-exp 'y))))))
                          test-env)
                7)))
