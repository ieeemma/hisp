
(define fac
  (lambda (n)
    (if (= n 0)
        1
        (* n (fac (- n 1))))))

(define-macro test (x y) x)

(test (print 5) (print 10))

(define car
  (lambda ((x . xs)) x))
(define cdr
  (lambda ((x . xs)) xs))

(print (car '(3 4 5)))

