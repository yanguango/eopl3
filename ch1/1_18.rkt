(define swapper
  (lambda (s1 s2 lst)
    (cond
     [(null? lst) '()]
     [(symbol? (car lst))
     (if (equal? s1 (car lst))
         (cons
          s2 
          (swapper s1 s2 (cdr lst)))
      (if (equal? s2 (car lst))
          (cons
           s1
           (swapper s1 s2 (cdr lst)))
          (cons
           (car lst)
           (swapper s1 s2 (cdr lst)))))]
     [(list? (car lst))
      (cons
       (swapper s1 s2 (car lst))
       (swapper s1 s2 (cdr lst)))])))
