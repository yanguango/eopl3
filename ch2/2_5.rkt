(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

;; empty-env : () → Env
;; usage: produce a representation of the empty environment
(define empty-env
  (lambda () '()))

;; extend-env : Var * SchemeVal * Env -> Env
;; usage: extend env with variable, and return extended environment
(define extend-env
  (lambda (var val env)
    (cons (list var val) env)))

;; app-env : Env * Var -> SchemeVal
;; usage : applies a representation of an environment to a variable
(define app-env
  (lambda (env search-var)
    (if (equal? env '())
        (report-no-binding-found search-var)
        (let ((saved-var (caar env))
              (saved-val (cadr (car env))))
          (if (equal? saved-var search-var)
              saved-val
              (app-env (cdr env) search-var))))))
