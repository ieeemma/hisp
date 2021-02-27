
(define-struct lam args body closure)

(define (eval_ exp env)
  (cond
    [(tagged? exp 'lambda)
       (unpack exp (_ args body) (make-lam args body env))]
    [(symbol? exp) _]
    [else exp]))
    
    
(print-line (eval_ '(lambda (x) y) 'a))
