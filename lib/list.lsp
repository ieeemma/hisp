
(define (index i xs)
  (if (zero? i)
      (car xs)
      (index (- i 1) (cdr xs))))

(define (length xs)
  (if (null? xs)
      0
      (+ 1 (length (cdr xs)))))

(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (add xs x) (append xs (list x)))

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

(define-macro (for name iter body)
  (list 'map (list 'lambda (list name) body) iter))
  
(define (take n xs)
  (if (or (<= n 0) (null? xs))
      '()
      (cons (car xs) (take (- n 1) (cdr xs)))))
      
(define (drop n xs)
  (if (or (<= n 0) (null? xs))
      xs
      (drop (- n 1) (cdr xs))))

(define (foldl f z xs)
  (if (null? xs)
      z
      (foldl f (f z (car xs)) (cdr xs))))

(define (foldr f z xs)
  (if (null? xs)
      z
      (foldr f (car xs) (foldr f z (cdr xs)))))

(define (partition p xs)
  (if (null? xs)
      (cons '() '())
      (let [(rest (partition p (cdr xs)))
            (x (car xs))
            (resta (car rest))
            (restd (cdr rest))]
           (if (p x)
               (cons (add resta x) restd)
               (cons resta (add restd x))))))

(define (filter f xs)
  (if (null? xs)
    '()
    (let [(x (car xs))
          (rest (filter f (cdr xs)))]
      (if (f x)
          (cons x rest)
          rest))))

(define (zip . xs)
  (error "zip aint working")
  (if (any null? xs)
      '()
      (cons (map car xs) (apply zip (map cdr xs)))))
      
(define (merge-sort-on f xs)
  (define (merge f xs ys)
    (cond [(null? xs) ys]
          [(null? ys) xs]
          [(f (car xs) (car ys))
             (cons (car xs) (merge f (cdr xs) ys))]
          [else
             (cons (car ys) (merge f xs (cdr ys)))]))

  (let [(len (length xs))]
    (cond [(= len 0) '()]
          [(= len 1) xs]
          [else 
            (let [(pivot (/ len 2))
                  (x  (merge-sort-on f (take pivot xs)))
                  (y  (merge-sort-on f (drop pivot xs)))]
              (merge f x y))])))
		 	    
(define (sort xs) (merge-sort-on < xs))

