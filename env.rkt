#lang racket

(provide env
         env?
         empty-env
         empty-env?
         env-syms
         env-vals
         env-previous
         env-lookup)
 
; The empty environment is null.
(define empty-env null)
(define empty-env? null?)

(struct env (syms vals previous) #:transparent)

(define (find-value syms vals symbol)
  (if (equal? symbol (first syms)) (first vals)
      (find-value (rest syms) (rest vals) symbol)))

(define (env-lookup environment symbol)
  (cond [(empty-env? environment) (error 'env-lookup "No binding for ~s" symbol)]
        [(member symbol (env-syms environment))
         (find-value (env-syms environment) (env-vals environment) symbol)]
        [else (env-lookup (env-previous environment) symbol)]))
      