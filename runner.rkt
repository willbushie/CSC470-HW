#lang racket
; CSC470-HW14-Code - William Bushie
; runner file

; imports 
(require "utility.rkt")

;resolve a value from variable environment
(define resolve_scope;((a 1) (b 2) (c 5)), it gives two kinds of result. found return a value
  ; not found return #false
  (lambda (scope varname)
    (cond
      ((null? scope) #false)
      ((equal? (caar scope) varname) (cadar scope))
      (else (resolve_scope (cdr scope) varname)) )))

;environment is a list of scopes
;global variable scope (global (a 1) (b 2) (c 5))
;local variable scope has no keywords as the first element
(define resolve_env
  (lambda (environment varname)
    (cond
      ((null? environment) #false)
      ((null? (car environment)) (resolve_env (cdr environment) varname))
      ((equal? 'global (car (car environment))) (resolve_scope (cdr (car environment)) varname))
      (else (let ((resolved_result (resolve_scope (car environment) varname)))
              (if (equal? resolved_result #false)
                  (resolve_env (cdr environment) varname) resolved_result))) )))
                                 
;it will be only called in let-exp
(define extend-scope
  (lambda (list-of-varname list-of-value scope)
    (cond
      ((null? list-of-varname) scope)
      ((null? list-of-value) scope)
      (else (extend-scope (cdr list-of-varname) (cdr list-of-value)
       (cons (list (car list-of-varname) (car list-of-value)) scope))) )))

(define push_scope_to_env
  (lambda (list-of-varname list-of-value env)
    ;construct a new scope based on list of varnames and list of values
    ;pop off scopes on top of global scope in environment
    ;concate the new scope to the global scope environment
    (let ((new_scope (extend-scope list-of-varname list-of-value '()))
          (pop_off_env (pop_env_to_global_scope env))) (cons new_scope pop_off_env) )))

;remove all scopes on top of global scope
(define pop_env_to_global_scope
  (lambda (env)
    (cond
      ((null? env) #false)
      ((equal? (length env) 1)
       (if (equal? (car (car env)) 'global) env #false))
      (else (pop_env_to_global_scope (cdr env))) )))

;add name value pairs to the local scope
(define extend_local_scope
  (lambda (list-of-varname list-of-value env)
    (cond
      ((null? env) #false)
      ;check the first scope is local scope or not
      ((equal? (caar env) 'global) (push_scope_to_env list-of-varname list-of-value env))
      ;use extend_scope function to add new variables into the local scope
      (else (cons (extend-scope list-of-varname list-of-value (car env)) (pop_env_to_global_scope env))) )))

; main runner
(define run-neo-parsed-code
  (lambda (parsed-code env)
    (cond
      ((null? parsed-code) '())
      ((equal? (car parsed-code) 'num-exp)
       (cadr parsed-code));(num-exp 22)
      ((equal? (car parsed-code) 'var-exp)
       (resolve_env env (cadr parsed-code)))
      ;(bool-exp op (neo-exp) (neo-exp))
      ((equal? (car parsed-code) 'bool-exp) (run-bool-parsed-code (cdr parsed-code) env))
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
      ;(app-exp (func-exp (params (x)) (body-exp (let-exp ((a 1) (b 2) (c 3)) (math-exp + (var-exp a) (var-exp b))))) ((num-exp 5)))
      ((equal? (car parsed-code) 'let-exp)
       (run-let-exp parsed-code env))
      (else (run-neo-parsed-code
             (cadr parsed-code) ;function expression
             (push_scope_to_env (cadr (cadr (cadr parsed-code)))
                                (map (lambda (exp) (run-neo-parsed-code exp env)) (caddr parsed-code)) env))) )))


; run bool parsed code
(define run-bool-parsed-code
  (lambda(parsed-code env)
    (let ((op (elementAt parsed-code 0))
           (num1 (run-neo-parsed-code (elementAt parsed-code 1) env))
           (num2 (run-neo-parsed-code (elementAt parsed-code 2) env)))
           (cond
             ((equal? op '>) (> num1 num2))
             ((equal? op '<) (< num1 num2))
             ((equal? op '>=) (>= num1 num2))
             ((equal? op '<=) (<= num1 num2))
             ((equal? op '==) (= num1 num2))
             ((equal? op '!=) (not (= num1 num2)))
             ((equal? op '&&) (and num1 num2))
             ((equal? op '||) (or num1 num2))
             (else (not num1))) )))

; run math parsed code
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

    
; run let parsed code
;(define run-let-exp
;  (lambda (parsed-code env)
;    ;((a (num-exp 7)) (b (var-exp a)) (x (var-exp b))) > ((a 7) (b 7) (x 7))
;    ;(a (num-exp 7)) -> (a 7) < (list (car code) (run-neo-parsed-code (cadr code) env))) 
;    (let* ((resolved-var-list (cascade-update-env (elementAt parsed-code 1) env))
;           (list-of-names (getVarnames (elementAt parsed-code 1)))
;           ;list-of-values = ((num-exp 7) (var-exp a) (var-exp b))
;           ;body = (math + (var-exp x) (var-exp a))
;          (list-of-values (getValues resolved-var-list))
;          (new_env (extend_local_scope list-of-names list-of-values env))
;          ;new variables will be added to the local scope
;          (body (elementAt parsed-code 2)))
;      ;(run-neo-parsed-code body new_env) )))
;      (display list-of-values) )))


; run let parsed code
(define run-let-exp
  (lambda (parsed-code env)
    (let* ((resolved-var-list
            (map (lambda (pair) (list (car pair) (run-neo-parsed-code (cadr pair) env))) (elementAt parsed-code 1)))
           (list-of-names (getVarnames (elementAt parsed-code 1)))
          (list-of-values (getValues resolved-var-list))
          (new_env (extend_local_scope list-of-names list-of-values env))
          (body (elementAt parsed-code 2)))
      (run-neo-parsed-code body new_env) )))

; update scope
;(define cascade-update-env
;  (lambda (parsed-scope env)
;    (cond
;      ((null? parsed-scope) env)
;      (let* ((original-local-scope (if (equal? (car (car env) 'global)) '() (car env)))
;           (curr-local-scope (extend_local_scope
;                              (list (car (car parsed-code)))
;                              (list (cadr (car parsed-code)))
;                              local-scope
;                             ))
;                 (pop_off_env (pop_env_to_global_scope env)))
;        (cascade-update-env (cdr parsed-scope)
;                            ((cons new_scope pop_off_env))))
;)))
          

(define cascade-update-env
  (lambda (parsed-scope env)
    (if (null? parsed-scope) env
        (let* ((original-local-scope (if (equal? (car (car env)) 'global) '() (car env)))
               (varname (caar parsed-scope))
               (var_value (run-neo-parsed-code (cadr (car parsed-scope)) env))
               (pop_off_env (pop_env_to_global_scope env))
               (new_env (list (cons (list varname var_value) original-local-scope) (pop_off_env)))) (cascade-update-env (cdr parsed-scope) new_env)) )))
          

; update scope
;(define cascade-update-env
;  (lambda (parsed-scope env)
;    (if (null? parsed-scope) env
;        (let* ((original-local-scope (if (equal? (car (car env)) 'global) '() (car env)))
;               (varname (caar parsed-scope))
;              (var_value (run-neo-parsed-code (cadr (car parsed-scope)) env))
;              (pop_off_env (pop_env_to_global_scope env))
;              (new_env (list (cons (list varname var_value) original-local-scope) (pop_off_env)))) (cascade-update-env (cdr parsed-scope) new_env))) ))














; provide all functions out
(provide (all-defined-out))