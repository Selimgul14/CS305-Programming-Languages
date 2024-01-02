(define get-operator (lambda (op-symbol)
  (cond
    ((eq? op-symbol '+) +)
    ((eq? op-symbol '*) *)
    ((eq? op-symbol '-) -)
    ((eq? op-symbol '/) /)
    (else (display "ERROR")))))


(define get-value (lambda (var env)
    (cond 
       ( (null? env) (display "ERROR") )
       ( (eq? var (caar env)) (cdar env))
       ( else (get-value var (cdr env))))))


(define extend-env (lambda (var val old-env)
        (cons (cons var val) old-env)))


(define define-expr? (lambda (e)
         (and (list? e) (= (length e) 3) (eq? (car e) 'define) (symbol?(cadr e)))))


(define if-expr? (lambda (e)
         (and (list? e) (= (length e) 4) (eq? (car e) 'if))))
         

(define cond?
  (lambda (e)
      (and (list? e) (eq? (car e) 'cond) (> (length e) 2)
           (if (> (length e) 3)
               (and (conditional? (cadr e)) (cond? (cons 'cond (cddr e))))
               (and (conditional? (cadr e)) (else-condition? (caddr e)))))))
   
(define valid-cond? 
  (lambda (e)
    (let ((last-clause (last e)))
      (and (cond? e) (else-condition? last-clause)))))


(define conditional? (lambda (e)
    (and (list? e) (= (length e) 2))))


(define else-condition? (lambda (e)
    (and (list? e) (= (length e) 2) (eq? (car e) 'else))))


(define selimgul-hw5 (lambda (e env)
   (cond
        ((number? e) e)
        ((symbol? e) (get-value e env))
        ((not (list? e)) (begin (display "ERROR: Invalid syntax - expected a list\n") e)) 
        ((define-expr? e)
            (if (number? (cadr e)) 
                (display "ERROR: Invalid syntax for define - expected a symbol as first argument\n") 
                (let ((val (s7 (caddr e) env)))
                    (if (null? val)
                        '()
                        (extend-env (cadr e) val env)))))
        ((if-expr? e)
            (if (< (length e) 4) 
                (display "ERROR: Invalid syntax for if - expected 3 arguments\n")
                (if (= (s7 (cadr e) env) 0)
                    (s7 (cadddr e) env)
                    (s7 (caddr e) env))))
        ((cond? e)
            (if (not (valid-cond? e))
                (display "ERROR: Invalid syntax for cond\n")
                (cond-eval e env)))
        (else
            (if (not (symbol? (car e)))
                (display "ERROR: Invalid operator - expected a symbol\n")
                (let ((operator (get-operator (car e)))
                      (operands (map s7 (cdr e) (make-list (length (cdr e) ) env )))
                      )
                    (apply operator operands)))))))

        
(define repl (lambda (env)
   (let* (
           (dummy1 (display "cs305> "))
           (expr (read))
           (new-env (if (define-expr? expr) 
                        (extend-env (cadr expr) (s7 (caddr expr) env) env)
                        env
                    ))
           (val (if (define-expr? expr)
                    (cadr expr)
                    (s7 expr env)
                ))
           (dummy2 (display "Value: "))
           (dummy3 (display val))
           (dummy4 (newline))
           (dummy5 (newline))
          )
          (repl new-env))))


(define cs305 (lambda () (repl '())))