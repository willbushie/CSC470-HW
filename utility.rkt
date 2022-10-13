#lang racket
; CSC470-HW13-Code - William Bushie
; utilty file (useful tools)


;resolve 
(define resolve
  (lambda (environment varname)
    (cond
      ((null? environment) #false)
      ((equal? (caar environment) varname) (cadar environment))
      (else (resolve (cdr environment) varname)) )))

;extend environment tool
;env = ((a 1) (b 2) (c 5)
(define extend-env
  (lambda (list-of-varname list-of-value env) ;((x y z) (1 2 3) env)
    (cond
      ((null? list-of-varname) env)
      ((null? list-of-value) env)
      (else (extend-env (cdr list-of-varname) (cdr list-of-value)
       (cons (list (car list-of-varname)
                   (car list-of-value)) env))) )))

;tool to help with math
(define run-math-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '+) (+ num1 num2)) ;+ 1 1)
      ((equal? op '-) (- num1 num2))  ;- 1 1
      ((equal? op '*) (* num1 num2)) ;* 1 1
      ((equal? op '/) (/ num1 num2))  ;/ 1 1 float division
      ((equal? op '//) (quotient num1 num2))  ;// 1 1 interger division
      ((equal? op '%) (modulo num1 num2))  ;% 1 1 modulo
      (else #false) )))

;tool to define bool-exp
(define run-bool-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '>) (> num1 num2))
      ((equal? op '<) (< num1 num2))
      ((equal? op '>=) (>= num1 num2))
      ((equal? op '<=) (<= num1 num2))
      ((equal? op '==) (= num1 num2))
      ((equal? op '!=) (not (= num1 num2)))
      ((equal? op '&&) (and num1 num2))
      ((equal? op '||) (or num1 num2))
      (else (not num1)) )))

;tool to find element at specified index
(define elementAt
  (lambda (lst index)
    (cond ((not (list? lst)) "This is not a list")
          ((null? lst) "This list is empty or index out of bounds")
          ((equal? index 0) (car lst))
          (else (elementAt (cdr lst) (- index 1))) )))

;(define env (a 1) (b 2) (c 3) (d 4)) -> (a b c d) + (1 2 3 4)
; get variable names from ((a 1) (b 2)) input
(define getVarnames
  (lambda (lst)
    (if (null? lst) '()
        (cons (car (car lst)) (getVarnames (cdr lst))) )))

; get values from ((a 1) (b 2)) input
(define getValues
  (lambda (lst)
    (if (null? lst) '()
        (cons (car (cdr (car lst))) (getValues (cdr lst))) )))





; provide all methods for outside use
(provide (all-defined-out))