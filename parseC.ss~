;(require (lib "eopl.ss" "eopl"))
(define-datatype expression expression?      
  (lit-exp (datum number?))
  (varref (var symbol?)))

;the parser simply creates a lit-expt when it sees a number (and throws an error otherwise). It looks like this
(define parse  
  (lambda (exp)            
    (cond            
      ((number? exp) (lit-exp exp)) 
      ((symbol? exp) (varref exp))         
      (else (error 'parse "Invalid concrete syntax ~s" exp)))))


;unparser is just as simple:

(define unparse     
  (lambda (tree)           
    (cases expression tree  
      (lit-exp (datum) datum)
      (varref (var) var))))
               