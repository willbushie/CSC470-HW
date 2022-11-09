#lang racket

; obtain element given index
(define elementAt
  (lambda(lst index)
    (cond
      ((not (list? lst)) "this is not a list")
      ((null? lst) "this is an empty list or index out of bound.")
      ((equal? index 0) (car lst))
      (else (elementAt (cdr lst) (- index 1))) )))

; get varnames from key value pairs list
(define getVarnames
  (lambda (lst)
    (if (null? lst) '()
        (cons (car (car lst)) (getVarnames (cdr lst))) )))

; get values from key value pairs list
(define getValues
  (lambda (lst)
    (if (null? lst) '()
        (cons (car (cdr (car lst))) (getValues (cdr lst))) )))

; provide all functions out
(provide (all-defined-out))