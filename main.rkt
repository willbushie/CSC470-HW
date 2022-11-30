#lang racket
; CSC470-HW17-Code - William Bushie
; main execution file

; imports
(require "utility.rkt")
(require "runner.rkt")
(require "parcer.rkt")
(require "variable_env.rkt")

; define global variable scope
(define env '((global (a 1) (b 2) (c 5))))

; define sample code
;(define sample-code '(local-vars ((p c)) (math / a p)))
;(define sample-code '(local-vars ((a 7) (b a) (x b)) (math + x a)))
(define sample-code '(block (assign i 0) (while (bool < i 10) (block (assign a (math + i 1)) (assign i a) (print i)))))

(displayln (neo-parser sample-code))

; test sample code
;(displayln (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)