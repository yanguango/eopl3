(define list-set-at
  (lambda (lst n x at)
    (if (null? lst) '()
        (if (equal? n at)
            (cons
             x
             (list-set-at (cdr lst) n x (+ at 1)))
            (cons
             (car lst)
             (list-set-at (cdr lst) n x (+ at 1)))))))
