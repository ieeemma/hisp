
(define (car (x . xs)) x)
(define (cdr (x . xs)) xs)

(define (cadr x) (car (cdr x)))

(define (list . xs) xs)

(define-macro (bool x) (list 'if x #t #f))

(define-macro (and x y) (list 'if x (list 'bool y) #f))
(define-macro (or x y) (list 'if x #t (list 'bool y)))
(define (not x) (if x #f #t))

(define (true? x) (eq? x #t))
(define (false? x) (eq? x #f))

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
      (all f (cdr xs))
      #f)))

(define (id x) x)

(define (print-line . xs)
  (map print xs)
  (print "\n"))
  
(define (input x)
  (do (print x) (read)))
  
(define (zero? x) (= x 0))

(define (<= x y) (or (= x y) (< x y)))
(define (>= x y) (or (= x y) (> x y)))

(define (sqrt x) (expt x (/ 1 2)))

(define (range i j)
  (if (= i j)
      '()
      (cons i (range (+ i 1) j))))

; ============== Macro forms ==============

(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs)))))

(define (tagged? x tag)
  (and (pair2? x) (eq? (car x) tag)))
  
(define (pair2? x)
  (and
    (pair? x)
    (and (pair? (cdr x)) (null? (cdr (cdr x))))))

(define (quasiquote-rec x)
  (if (tagged? x 'unquote)
    (cadr x)
    (if (pair? x)
        (list 'cons (quasiquote-rec (car x))
                    (quasiquote-rec (cdr x)))
        (if (symbol? x)
            (list 'quote x)
            x))))
        
(define-macro (quasiquote x)
  (quasiquote-rec x))
  
(define-macro (cond . cases)
  (cond-rec cases))

(define (cond-rec cases)
  (if (null? cases)
    '()
    (let [(case (car cases))]
      (if (not (pair2? case))
          (error "Malformed cond case" case)
          (if (eq? (car case) 'else)
              (cadr case)
              `(if ,(car case)
                   ,(cadr case)
                   ,(cond-rec (cdr cases))))))))

(define-macro (unpack x case body)
  `((lambda (,case) ,body) ,x))
