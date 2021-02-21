
(define (car (x . xs)) x)
(define (cdr (x . xs)) xs)
(define (list . xs) xs)

(define (apply fn args)
  (eval (cons fn args)))

(define (not x) (if x #f #t))
(define (and x y) (if x y #f))
(define (or  x y) (if x #t y))

(define (id x) x)

(define (print-line x)
  (print
    (<> (if (string? x) x (show x)) "\n")))
  
(define (input x)
  (do (print x) (read)))
