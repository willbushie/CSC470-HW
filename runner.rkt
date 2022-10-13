#lang racket
; CSC470-HW13-Code - William Bushie
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
      ;'(app-exp (func-exp (params (x)) (body-exp (let-exp ((a 1) (b 2) (c 3)) (math-exp + (var-exp a) (var-exp b))))) ((num-exp 5)))
      ((equal? (car parsed-code) 'let-exp) (run-let-exp parsed-code env))
      (else (run-neo-parsed-code
             ;function expression
             (cadr parsed-code)
             (extend-env
              (cadr (cadr (cadr parsed-code)))
              ;list of values ((num-exp 1) (var-exp a) (math-exp + (num-exp 2) (num-exp 3)))
              ;environment scope update
              (map (lambda (exp) (run-neo-parsed-code exp env)) (caddr parsed-code)) env))) )))


; run let expression
(define run-let-exp
  (lambda (parsed-code env)
    (let* ((list-of-names (getVarnames (elementAt parsed-code 1)))
          (list-of-values (getValues (elementAt parsed-code 1)))
          (new_env (extend-env list-of-names list-of-values env))
          (body (elementAt parsed-code 2)))
    (run-neo-parsed-code body new_env) )))



; provide all methods for outside use
(provide (all-defined-out))