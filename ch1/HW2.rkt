;; 1.17
;; down: ListOf(SchemeVal) -> ListOf(SchemeVal)
;; usage: (down lst) wraps parentheses around each top-level element of lst

(define down
  (lambda (lst)
    (if (null? lst) '()
        (cons
         (list (car lst))
         (down (cdr lst))))))

;; 1.18
;; swapper: SchemeVal * SchemeVal * ListOf(SchemeVal) -> ListOf(SchemeVal)
;; returns a list the same as slist, but with all occurrences of s1 replaced by s2 and all occurrences of s2 replaced by s1

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

;; 1.19
;; list-set: ListOf(SchemeVal) * Int * SchemeVal * Int -> ListOf(SchemeVal)
;; returns a list like lst,except that the n-th element, using zero-based indexing, is x
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

;; list-set: ListOf(SchemeVal) * Int * SchemeVal -> ListOf(SchemeVal)
;; returns a list like lst,except that the n-th element, using zero-based indexing, is x
(define list-set
  (lambda (lst n x)
    (list-set-at lst n x 0)))


;; 1.24
;; every?: SchemeVal * ListOf(SchemeVal) -> Bool
;; returns #f if any element of lst fails to satisfy pred, and returns #t otherwise.
(define every?
  (lambda (pred lst)
    (if (null? lst) #t
        (and (apply pred (list (car lst)))
             (every? pred (cdr lst))))))

;; 1.27
;; flatten: ListOf(SchemeVal) -> ListOf(SchemeVal)
;; returns a list of the symbols contained in slist in the order in which they occur when slist is printed
(define flatten
  (lambda (slist)
    (if (null? slist) '()
        (if (list? (car slist))
            (append (flatten (car slist))
                    (flatten (cdr slist)))
            (cons
             (car slist)
             (flatten (cdr slist)))))))
