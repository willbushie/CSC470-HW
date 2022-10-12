#lang racket
; CSC470 - HW 11 - William Bushie
; runner file

; imports
(require "parser.rkt" "utility.rkt")

;run neo parsed code
;(app-exp (func-exp (params x) (body-exp (var-exp x))) (var-exp a))
(define run-neo-parsed-code
  (lambda (parsed-code env)
    (cond
      ((null? parsed-code) '())
      ((equal? (car parsed-code) 'num-exp)
       (cadr parsed-code));(num-exp 22)
      ((equal? (car parsed-code) 'var-exp)
       (resolve env (cadr parsed-code)))
      ;(bool-exp op (neo-exp) (neo-exp))
      ((equal? (car parsed-code) 'bool-exp)
       (run-bool-exp (cadr parsed-code)
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env)))
      ;(math-exp op (neo-exp) (neo-exp))
      ((equal? (car parsed-code) 'math-exp)
       (run-math-exp (cadr parsed-code)
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'ask-exp)
       (if (run-neo-parsed-code (cadr parsed-code) env)
           (run-neo-parsed-code (caddr parsed-code) env)
           (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'func-exp)
       (run-neo-parsed-code (cadr (caddr parsed-code)) env))
      (else (run-neo-parsed-code
             ;function expression
             (cadr parsed-code)
             (extend-env
              (cadr (cadr (cadr parsed-code)))
              ;list of values ((num-exp 1) (var-exp a) (math-exp + (num-exp 2) (num-exp 3)))
              ;environment scope update
              (map (lambda (exp) (run-neo-parsed-code exp env)) (caddr parsed-code)) env))) )))

; provide all methods for outside use
(provide (all-defined-out))