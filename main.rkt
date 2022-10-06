#lang racket
; CSC470-HW11-Code - William Bushie
; main execution code file

; imports
(require "parser.rkt")
(require "runner.rkt")
(require "utility.rkt")




(define env '((a 1) (b 2) (c 5)))
;(app-exp (func-exp (params (identifier1, identifier2, identifer3 ...)) (body-exp)) ((neo-exp1 neo-exp2 neo-exp3 ...))
(define sample-code '(call (function () (ask (bool > a b) (math - a b) (math + a b))) (a)))
(display (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)

