#lang racket
; CSC470 - HW 11 - William Bushie
; main execution code file

; imports
(require "parser.rkt")
(require "runner.rkt")
(require "utility.rkt")
(require "var_env.rkt")




;(define env '((a 1) (b 2) (c 5)))
;(app-exp (func-exp (params (identifier1, identifier2, identifer3 ...)) (body-exp)) ((neo-exp1 neo-exp2 neo-exp3 ...))
;(define sample-code '(call (function () (ask (bool > a b) (math - a b) (math + a b))) (a)))
;(display (neo-parser sample-code))
;(define parsed-neo-code (neo-parser sample-code))
;(run-neo-parsed-code parsed-neo-code env)

;(define env (a 1) (b 2) (c 3) (d 4)) -> (a b c d) + (1 2 3 4)
(define env '((a 1) (b 2) (c 3) (d 4)))

;(local-var ((a 1) (b 2) (c 3)) (neo-exp))
;(neo-let-code-parser '(local-var ((a 1) (b 2) (c 3)) (math + a b)))
;(neo-parser '(local-vars ((a 1) (b 2) (c 3)) (math + a b)))

;(call (function (a) (local-vars ((x 5) (y 6) (z 9)) ((call (function (b)(math + a (math * b x)))) (2)))) (3))
(neo-parser '(call (function (b) (local-vars ((a 2) (b 4) (c 5)) (math + a b))) (5) (7)))



