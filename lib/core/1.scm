
(define (car (x . xs)) x)
(define (cdr (x . xs)) xs)

(define (not x) (if x #f #t))
(define (and x y) (if x y #f))
(define (or  x y) (if x #t y))

(define (id x) x)
