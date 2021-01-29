
(define-struct vec-2d x y)

(define v (make-vec-2d 5 10))

(print (vec-2d-x v))
(vec-2d-x-set! v 100)
(print (vec-2d-x v))

(define-macro (test x y)
  (print "hi"))

(define x 5)
(print x)

(define (foo x) (* 2 x))
(print (foo 5))

