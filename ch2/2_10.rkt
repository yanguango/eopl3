;; 2.10
;; extend-env* ListOf(Var) * ListOf(Val) -> Env
(define extend-env*
  (lambda (var-list val-list env)
    (if (null? var-list) env
        (cons (list (car var-list) (car val-list))
              (extend-env* (cdr var-list) (cdr val-list) env)))))
