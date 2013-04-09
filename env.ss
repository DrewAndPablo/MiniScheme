;(require (lib "eopl.ss" "eopl"))
(define-datatype environment environment? 
       (empty-env) 
       (extended-env
              (syms (list-of symbol?))
              (vals (list-of anything?))
              (env environment?)))

(define anything? (lambda (v) #t))
(define atom?
  (lambda (a)
    (cond
      [(null? a) #f ]
      [(pair? a) #f ]
      [else #t])))
(define the-empty-env (empty-env))
(define EnvA (extended-env '(x y) '(1 2) the-empty-env))
(define EnvB (extended-env '(x z) '(5 7) EnvA))
(define member*
  (lambda(a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a)
           (member* a (cdr l))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))
(define nth
  (lambda (n lat)
    (cond
      [(= 0 n) (car lat)]
      [else (nth (- n 1) (cdr lat))])))
(define index
  (lambda (a lat)
    (cond
      [(null? lat) -1]
      [(equal? a (car lat)) 0]
      [else (my-increment (index a (cdr lat)))])))
(define my-increment
  (lambda (x)
    (if (= x -1) -1 (+ 1 x))))
(define apply-env 
  (lambda (E sym)
    (cases  environment E
      (empty-env () (error 'apply-env "No binding for ~s" sym))
      (extended-env (syms vals env)
                    (if (member* sym syms) 
                        (nth (index sym syms) vals)
                        (apply-env env sym))))))

(define-datatype proc proc?
       (prim-proc (prim-op prim-op?))
       (closure (params (list-of symbol?))
                       (body expression?)
                       (env environment?)))
(define apply-proc (lambda (p args)
       (cases proc p
              (prim-proc (prim-op)
                     (apply-primitive-op prim-op args))
              (else (error 'apply-proc "Bad procedure: ~s" p)))))
(define apply-primitive-op (lambda (op args)
       (cond
              [(eq? op '+) (+ (car args) (cadr args))]
              [(eq? op '-) (- (car args) (cadr args))]
              [(eq? op '*) (* (car args) (cadr args))]
              [(eq? op '/) (/ (car args) (cadr args))])))

(define prim-op? 
        (lambda (sym) (member? sym prim-op-names)))
(define prim-op-names '(+ - * /))
(define fold
  (lambda (recur base lat)
    (letrec ([h (lambda (s)
                  (cond
                    [(null? s) base]
                    [else (recur (car s) (h (cdr s)))]))])
                  (h lat))))
(define member? (lambda (a lat)
    (fold (lambda (x y)
            (if (equal? x a) #t y))
          #f lat)))
(define init-env 
        (extended-env 
               prim-op-names 
               (map prim-proc prim-op-names) 
               (extended-env '(x y) '(1 2) the-empty-env)))

                        
                        