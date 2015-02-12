#lang eopl
(define (report-invalid-concrete-syntax datum)
  (eopl:error 'parse-expression
    "invalid concrete syntax: ~A" datum))

(define identifier? symbol?)

(define-datatype lc-exp lc-exp? (var-exp
      (var identifier?))
    (lambda-exp
      (bound-var identifier?)
      (body lc-exp?))
    (app-exp
      (rator lc-exp?)
      (rand lc-exp?)))

(define parse-expression
    (lambda (datum)
      (cond
((symbol? datum) (var-exp datum)) ((pair? datum)
(if (eqv? (car datum) 'lambda) 
    (lambda-exp
     (car (cadr datum))
     (parse-expression (caddr datum))) 
    (app-exp
(parse-expression (car datum))
(parse-expression (cadr datum)))))
(else (report-invalid-concrete-syntax datum)))))

(define unparse-lc-exp
    (lambda (exp)
      (cases lc-exp exp
(var-exp (var) var) (lambda-exp (bound-var body)
          (list 'lambda (list bound-var)
            (unparse-lc-exp body)))
        (app-exp (rator rand)
          (list
           (unparse-lc-exp rator) (unparse-lc-exp rand))))))
