
(define (car (x . xs)) x)
(define (cdr (x . xs)) xs)
(define (list . xs) xs)

(define (apply fn args)
  (eval (cons fn args)))

(define (not x) (if x #f #t))
(define (and x y) (if x y #f))
(define (or  x y) (if x #t y))

(define (any f xs)
  (if (null? xs)
    #f
    (if (f (car xs))
      #t
      (any f (cdr xs)))))

(define (all f xs)
  (if (null? xs)
    #t
    (if (f (car xs))
      (any (cdr xs))
      #f)))

(define (id x) x)

(define (print-line x)
  (print
    (<> (if (string? x) x (show x)) "\n")))
  
(define (input x)
  (do (print x) (read)))
