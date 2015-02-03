;; Author: Guang Yang
;; Assignment: Homework 1

;; I pledge my honor that I have abided by the Stevens Honor System.

;; Exercise 1.11
;; Because subst-in-s-exp calls subst, subst guaranteed termination because
;; it calls subst-in-s-exp in car of slist, and subst in smaller s-expression list.


;; Exercise 1.12
#lang eopl

(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
         (if (symbol? (car slist))
             (if (eqv? (car slist) old) new (car slist))
             (subst new old (car slist)))
         (subst new old (cdr slist))))))
