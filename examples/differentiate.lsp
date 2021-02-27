(load "lib/list.lsp")

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
  
(define (sum x y)
  (cond [(eq? x 0) y]
        [(eq? y 0) x]
        [else `(+ ,x ,y)]))
  
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
  
(define (product x y)
  (cond [(eq? x 0) 0]
        [(eq? x 1) y]
        [(eq? y 0) 0]
        [(eq? y 1) x]
        [else `(* ,x ,y)]))

(define (lhs x) (index 1 x))
(define (rhs x) (index 2 x))

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(symbol? exp)
         (if (eq? exp var) 1 0)]
        [(sum? exp)
         (sum (deriv (lhs exp) var)
              (deriv (rhs exp) var))]
        [(product? exp)
         (sum (product
                (lhs exp)
                (deriv (rhs exp) var))
              (product
                (deriv (lhs exp) var)
                (rhs exp)))]
        (else
         (error "unknown expression type" exp))))
         
(print-line (deriv '(+ (* y y) (* x y)) 'y))
