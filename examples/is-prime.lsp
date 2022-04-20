(define (prime? n)
  (let [(lower 2)
        (upper (+ 1(round (sqrt n))))]
  (all true?
    (for x (range lower upper)
      (not (zero? (% n x)))))))
      
(print-line (prime? 456789877))
