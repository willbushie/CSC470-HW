#lang racket
(require "utility.rkt")
(require "runner.rkt")
(require "parcer.rkt")
(require "variable_env.rkt")

;public static void main(){
;    int a = 1;
;    int b = 2;
;    int c = 5;

;    func1(a);

;    func2(a); //return 0.01

;    func_alex(a, 100); //return 0.01
;}

;function func1(int r){//r = a, and r = 1
;    return r;//return 1;
;}

;function func2(int r){//convert a number into percentage ratio
;    int p = 100;
;    int r = 5;
;    return r / p;
;}

;function func_alex(int r, int p){
;    return r / p;
;}

(define env '((global (a 1) (b 2) (c 5))))

;(define sample-code '(call (function (r) (local-vars ((p 100)) (math / r p)) ) (a)))
(define sample-code '(local-vars ((a 7) (b a) (x b)) (math + x a)))
(displayln (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)

