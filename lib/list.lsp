
(define (length xs)
  (if (null? xs)
      0
      (+ 1 (length (cdr xs)))))
      
(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))
      
(define (concat xs)
  (apply append xs))
  
(define (reverse xs)
  (if (null? xs)
      '()
      (append (reverse (cdr xs)) (list (car xs)))))
      
(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs)))))
      
(define (filter f xs)
  (let [(x (car xs))
        (rest (filter f (cdr xs)))]
  (if (f x)
      (cons x rest)
      rest))) 
