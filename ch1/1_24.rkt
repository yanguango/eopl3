(define every?
  (lambda (pred lst)
    (if (null? lst) #t
        (and (apply pred (list (car lst)))
             (every? pred (cdr lst))))))
