#lang racket

; imports
(require "utility.rkt")

; main parser
(define neo-parser
  (lambda (neo-code)
    (cond
      ((null? neo-code) '())
      ((number? neo-code) (list 'num-exp neo-code))
      ((symbol? neo-code) (list 'var-exp neo-code))
      ;(bool op num1 num2) > (bool-exp op (neo-exp) (neo-exp))
       ((equal? (car neo-code) 'bool) (neo-bool-code-parser neo-code))
      ;(math op num1 num2) > (math-exp op (neo-exp) (neo-exp))
      ((equal? (car neo-code) 'math) (neo-math-code-parser neo-code))
      ;(ask (bool op num1 num2) (neo-exp1) (neo-exp2)) > (ask-exp (bool-exp ...) (parsed-neo-exp1) (parsed-neo-exp2))
      ((equal? (car neo-code) 'ask) (neo-ask-code-parser neo-code))
      ((equal? (car neo-code) 'function) (neo-function-code-parser neo-code))
      ;(call (function (x y z) (math + (math + x y) z)) (1 2 3)) ->
      ;(app-exp (func-exp (params (identifier1, identifier2, identifer3 ...)) (body-exp)) ((neo-exp1 neo-exp2 neo-exp3 ...))
      ((equal? (car neo-code) 'call) (neo-call-code-parser neo-code))
      ((equal? (car neo-code) 'local-vars) (neo-let-code-parser neo-code))
      ;((neo-parser 1) (neo-parser 'a) (neo-parser (math + 1 2)))
      (else (map neo-parser neo-code))))) 

; parser for bool-exp
(define neo-bool-code-parser
  (lambda (neo-code)
     (if (equal? (length neo-code) 3)
            (list 'bool-exp (elementAt neo-code 1) (neo-parser (caddr neo-code)) '())
        (cons 'bool-exp (cons (cadr neo-code) (map neo-parser (cddr neo-code)))))))

; parser for math-exp
(define neo-math-code-parser
  (lambda (neo-code)
    (list 'math-exp (cadr neo-code)
             (neo-parser (caddr neo-code))
             (neo-parser (cadddr neo-code)))))

; parser for func-exp
(define neo-function-code-parser
  (lambda (neo-code)
    (list 'func-exp
             (list 'params (cadr neo-code))
             (list 'body-exp (neo-parser (caddr neo-code))))))

; parser for ask-exp
(define neo-ask-code-parser
  (lambda (neo-code)
    (cons 'ask-exp
             (map neo-parser (cdr neo-code)))))

; parser for call-exp
(define neo-call-code-parser
  (lambda(neo-code)
    (list 'app-exp
             (neo-parser (cadr neo-code))
             (neo-parser (caddr neo-code)))))

;(local-vars ((a 1) (b 2) (c a)) (neo-exp)) = neo-code
;(let-exp ((a 1) (b 2) (c 3)) (parsed-neo-exp))
;(let-exp ((a (num-exp 1)) (b (num-exp 2)) (c (var-exp a))) (parsed-neo-code))
;1 -> (num-exp 1) = code -> (neo-parser code)
;(a 1) -> (a (num-exp 1)) -> code == (a 1) < (list (car code) (neo-parser (cadr code)))
;((a 1) (b 2) (c a)) -> ((a (num-exp 1)) (b (num-exp 2)) (c (var-exp a)))
;((map (lambda (pair) (list (car pair) (neo-parser (cadr pair))) lst)

; parser for let-exp
(define neo-let-code-parser
  (lambda (neo-code)
    (list 'let-exp
          (map (lambda (pair) (list (car pair) (neo-parser (elementAt pair 1))))
               (elementAt neo-code 1))
           (neo-parser (elementAt neo-code 2)))))



; provide all functions out
(provide (all-defined-out))