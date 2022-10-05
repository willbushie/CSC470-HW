#lang racket
; CSC470-HW11-Code - William Bushie
; parser file

; imports
(require "runner.rkt" "CSC470-HW11-Code")

;neo parser
(define neo-parser
  (lambda (neo-code)
    (cond
      ((null? neo-code) '())
      ((number? neo-code) (list 'num-exp neo-code))
      ((symbol? neo-code) (list 'var-exp neo-code))
      ;(bool op num1 num2) > (bool-exp op (neo-exp) (neo-exp))
       ((equal? (car neo-code) 'bool)
        (if (equal? (length neo-code) 3)
            (list 'bool-exp (cadr neo-code) (neo-parser (caddr neo-code)) '())
        (cons 'bool-exp (cons (cadr neo-code) (map neo-parser (cddr neo-code))))))
      ;(math op num1 num2) > (math-exp op (neo-exp) (neo-exp))
      ((equal? (car neo-code) 'math)
       (list 'math-exp (cadr neo-code)
             (neo-parser (caddr neo-code))
             (neo-parser (cadddr neo-code))))
      ;(ask (bool op num1 num2) (neo-exp1) (neo-exp2)) > (ask-exp (bool-exp ...) (parsed-neo-exp1) (parsed-neo-exp2))
      ((equal? (car neo-code) 'ask)
       (cons 'ask-exp
             (map neo-parser (cdr neo-code))))
      ;(function (x y z,...) x)
      ((equal? (car neo-code) 'function)
       (list 'func-exp
             (list 'params (cadr neo-code))
             (list 'body-exp (neo-parser (caddr neo-code)))))
      ;(call (function (x y z) (math + (math + x y) z)) (1 2 3)) ->
      ;(app-exp (func-exp (params (identifier1, identifier2, identifer3 ...)) (body-exp)) ((neo-exp1 neo-exp2 neo-exp3 ...))
      ((equal? (car neo-code) 'call)
       (list 'app-exp
             (neo-parser (cadr neo-code))
             (neo-parser (caddr neo-code))))
      (else (map neo-parser neo-code)) ))) ;((neo-parser 1) (neo-parser 'a) (neo-parser (math + 1 2)))

; provide all methods for outside use
(provide (all-defined-out))