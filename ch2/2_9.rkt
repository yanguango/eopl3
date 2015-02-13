;; 2.9
;; has-binding? : Env * Val -> Bool
;; usage : see if s has an associated value in env
(define has-binding?
  (lambda (env s)
    (if (equal? env '()) #f
        (let ((saved-var (caar env))
              (saved-val (cadr (car env))))
          (if (equal? saved-var s) #t
              (has-binding? (cdr env) s))))))
