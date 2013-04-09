;(require	(lib "eopl.ss" "eopl"))
(define eval-exp     
  (lambda (tree env) 
    (cases expression tree 
      (lit-exp (datum) datum) 
      (varref (var) (apply-env env var))
      (app-exp (rator rands) (apply-proc rator rands))
      (else (error 'eval-exp  "Invalid abstract syntax: ~s" tree)))))
