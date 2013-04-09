;(require	(lib "eopl.ss" "eopl"))
(define eval-exp 
        (lambda (tree env) 
               (cases expression tree 
                      (lit-exp (datum) datum) 
                      (else (error 'eval-exp  "Invalid abstract syntax: ~s" tree)))))
