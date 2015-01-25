#lang eopl
;; (define subst
;;   (lambda (new old slist)
;;     (if (null? slist)
;;         '()
;;         (cons
;;          (subst-in-s-exp new old (car slist))
;;          (subst new old (cdr slist))))))

;; (define subst-in-s-exp
;;   (lambda (new old sexp)
;;     (if (symbol? sexp)
;;         (if (eqv? sexp old) new sexp)
;;         (subst new old sexp))))

(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
         (if (symbol? (car slist))
             (if (eqv? (car slist) old) new (car slist))
             (subst new old (car slist)))
         (subst new old (cdr slist))))))

(write (subst 'a 'b '(b (b c) ((b)))))
