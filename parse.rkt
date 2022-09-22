#lang racket

(provide lit-exp
         lit-exp?
         lit-exp-num
         var-exp
         var-exp?
         var-exp-symbol
         prim-proc
         prim-proc?
         prim-proc-op
         app-exp
         app-exp?
         app-exp-proc
         app-exp-args
         ite-exp
         ite-exp?
         ite-exp-cond
         ite-exp-then
         ite-exp-else
         let-exp
         let-exp?
         let-exp-syms
         let-exp-exps
         let-exp-body
         lambda-exp
         lambda-exp?
         lambda-exp-params
         lambda-exp-body
         set-exp
         set-exp?
         set-exp-sym
         set-exp-exp
         begin-exp
         begin-exp?
         begin-exp-exps
         parse)


(struct lit-exp (num) #:transparent)
(struct var-exp (symbol) #:transparent)
(struct prim-proc (op) #:transparent)
(struct app-exp (proc args) #:transparent)
(struct ite-exp (cond then else) #:transparent)
(struct let-exp (syms exps body) #:transparent)
(struct lambda-exp (params body) #:transparent)
(struct set-exp (sym exp) #:transparent)
(struct begin-exp (exps) #:transparent)

(define (parse input)
  (letrec ([parse-error (λ () (error 'parse "Invalid syntax ~s" input))])
    (cond [(number? input) (lit-exp input)]
          [(symbol? input) (var-exp input)]
          [(list? input)
           (cond [(empty? input) (parse-error)]
                 [(eq? (first input) 'if) (if (= (length input) 4)
                                              (ite-exp
                                               (parse (second input))
                                               (parse (third input))
                                               (parse (fourth input)))
                                              (parse-error))]
                 [(eq? (first input) 'let) (if (= (length input) 3)
                                               (if (valid-bind? (second input))
                                                   (let-exp
                                                    (map first (second input))
                                                    (map parse (map second (second input)))
                                                    (parse (third input)))
                                                   (parse-error))
                                               (parse-error))]
                 [(eq? (first input) 'lambda) (if (= (length input) 3)
                                                  (lambda-exp
                                                   (second input)
                                                   (parse (third input)))
                                                  (parse-error))]
                 [(eq? (first input) 'set!) (if (= (length input) 3)
                                                (set-exp
                                                 (second input)
                                                 (parse (third input)))
                                                (parse-error))]
                 [(eq? (first input) 'begin) (begin-exp (map parse (rest input)))]
                 [(eq? (first input) 'letrec) (if (= (length input) 3)
                                                  (parse-letrec input)
                                                  (parse-error))]
                 [else (app-exp (parse (first input))
                                (map parse (rest input)))])]
          [else (parse-error)])))

(define (valid-bind? input)
  (cond [(empty? input) #t]
        [(= (length (first input)) 2) (valid-bind? (rest input))]
        [else #f]))

(define (parse-letrec input)
  (let ([syms (map first (second input))]
        [exps (map second (second input))]
        [body (third input)])
    (let-exp syms
             (map (λ (s) (lit-exp 0)) syms)
             (let ([new-syms (map (λ (s) (gensym)) syms)])
                   (let-exp new-syms
                            (map parse exps)
                            (begin-exp
                              (foldr (λ (s new-s acc)
                                       (cons (set-exp s (parse new-s)) acc))
                                       (list (parse body))
                                       syms
                                       new-syms)))))))
