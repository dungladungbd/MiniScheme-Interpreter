#lang racket

(require rackunit)
(require "parse.rkt")

(provide parse-tests)

(define parse-tests
  (test-suite
   "Parse tests"
   (test-pred "Literal"
              lit-exp?
              (parse 5))

   (test-equal? "Literal value"
                (lit-exp-num (parse 10))
                10)

   (test-pred "Variable"
              var-exp?
              (parse 'x))

   (test-equal? "Variable symbol"
                (var-exp-symbol (parse 'x))
                'x)

   (test-equal? "Variable equality"
                (parse 'y)
                (var-exp 'y))

   (test-pred "Application, no args"
              app-exp?
              (parse '(foo)))

   (test-pred "Application, 1 args"
              app-exp?
              (parse '(add1 10)))

   (test-pred "Application, 3 args"
              app-exp?
              (parse '(+ 10 100 1000)))

   (test-equal? "Application proc"
                (app-exp-proc (parse '(+ 10 100)))
                (var-exp '+))

   (test-equal? "Application args (empty)"
                (app-exp-args (parse '(foo)))
                empty)
      
   (test-equal? "Application args (nonempty)"
                (app-exp-args (parse '(* 5 6)))
                (list (lit-exp 5) (lit-exp 6)))

   (test-equal? "Application proc is application"
                (parse '((foo) x))
                (app-exp (app-exp (var-exp 'foo) '()) (list (var-exp 'x))))

   (test-equal? "Application args is application"
                (parse '(foo (+ x y)))
                (app-exp (var-exp 'foo) (list (app-exp (var-exp '+) (list (var-exp 'x) (var-exp 'y))))))

   (test-pred "If-then-else"
              ite-exp?
              (parse '(if foo 1 2)))

   (test-equal? "If-then-else condition is application"
                (parse '(if (foo) 3 y))
                (ite-exp (app-exp (var-exp 'foo) '()) (lit-exp 3) (var-exp 'y)))

   (test-equal? "If-then-else then is application"
                (parse '(if x (foo 2) y))
                (ite-exp (var-exp 'x) (app-exp (var-exp 'foo) (list (lit-exp 2))) (var-exp 'y)))

   (test-equal? "If-then-else else is application"
                (parse '(if x 5 (foo)))
                (ite-exp (var-exp 'x) (lit-exp 5) (app-exp (var-exp 'foo) '())))

   (test-pred "Let"
              let-exp?
              (parse '(let ([a 10] [b 5]) (+ a b))))

   (test-equal? "Let expression is application"
                (parse '(let ([a 10] [b (+ 7 8)]) (+ a b)))
                (let-exp '(a b)
                         (list (lit-exp 10) (app-exp (var-exp '+) (list (lit-exp 7) (lit-exp 8))))
                         (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b)))))

   (test-equal? "Let inside of Let"
                (parse '(let ([a 6]
                              [b 24])
                          (let ([c (- b a)])
                            (* c b))))
                (let-exp '(a b)
                         (list (lit-exp 6) (lit-exp 24))
                         (let-exp '(c)
                                  (list (app-exp (var-exp '-) (list (var-exp 'b) (var-exp 'a))))
                                  (app-exp (var-exp '*) (list (var-exp 'c) (var-exp 'b))))))

   (test-pred "Lambda expression"
              lambda-exp?
              (parse '(lambda (x y) (+ x y))))
   
   (test-equal? "Lambda expression with arguments"
                (parse '((lambda (x y) (+ x y)) 3 5))
                (app-exp (lambda-exp '(x y)
                                     (app-exp (var-exp '+)
                                              (list (var-exp 'x)
                                                    (var-exp 'y))))
                         (list (lit-exp 3)
                               (lit-exp 5))))

   (test-equal? "Lambda expression inside let"
                (parse '(let ([f (lambda (x) (* 2 x))])
                          (f 6)))
                (let-exp '(f)
                         (list (lambda-exp
                                '(x)
                                (app-exp (var-exp '*)
                                         (list (lit-exp 2) (var-exp 'x)))))
                         (app-exp (var-exp 'f)
                                  (list (lit-exp 6)))))

   (test-pred "Set expression"
              set-exp?
              (parse '(set! x 23)))

   (test-equal? "Set"
                (parse '(set! x 23))
                (set-exp 'x (lit-exp 23)))
   
   (test-pred "Begin expression"
              begin-exp?
              (parse '(begin (set! x 23)
                             (+ x y))))

   (test-equal? "Begin"
                (parse '(begin (set! x 23)
                               (+ x y)))
                (begin-exp (list (set-exp 'x (lit-exp 23))
                                 (app-exp (var-exp '+) (list (var-exp 'x) (var-exp 'y))))))

   (test-equal? "Begin inside let"
                (parse '(let ([x 1] [y 2])
                          (begin (set! x 23)
                                 (+ x y))))
                (let-exp
                 '(x y)
                 (list (lit-exp 1) (lit-exp 2))
                 (begin-exp (list (set-exp 'x (lit-exp 23))
                                  (app-exp (var-exp '+) (list (var-exp 'x) (var-exp 'y)))))))

   (test-pred "Letrec"
              let-exp?
              (parse '(letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 4))))

   (test-pred "Body of let-exp of Letrec"
              let-exp?
              (let-exp-body (parse '(letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 4)))))

   (test-pred "Body of inner let-exp of Letrec"
              begin-exp?
              (let-exp-body (let-exp-body (parse '(letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 4))))))

   (test-pred "Exps of body of inner let-exp of Letrec"
              set-exp?
              (first (begin-exp-exps (let-exp-body (let-exp-body (parse '(letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 4))))))))
   
   (test-exn "Invalid syntax ()"
             exn:fail?
             (λ () (parse '())))

   (test-exn "Invalid syntax \"string\""
             exn:fail?
             (λ () (parse "string")))))
