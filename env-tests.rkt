#lang racket

(require rackunit rackunit/text-ui rackunit/gui)
(require "env.rkt")

(provide env-tests)

; Define an environment for testing.
(define test-env
  (env '(x y)
       '(1 2)
       empty-env))

(define test-env2
  (env '(x z)
       '(3 5)
       test-env))

(define env-tests
  (test-suite
   "Environment tests"
   (test-equal? "Symbol that’s bound in an environment"
                (env-lookup test-env 'x)
                1)

   (test-equal? "Symbol that’s bound in an environment and also in the environment’s previous environment"
                (env-lookup test-env2 'x)
                3)

   (test-equal? "Symbol that’s not bound in an environment but is bound in the environment’s previous environment"
                (env-lookup test-env2 'y)
                2)

   (test-exn "Symbol in an empty environment"
             exn:fail?
             (λ () (env-lookup empty-env 'x)))

   (test-exn "Symbol that’s not bound in an environment"
             exn:fail?
             (λ () (env-lookup test-env 'z)))))

(define all-tests
  (test-suite
   "All tests"
   env-tests))
