(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(print-line
  (let [(x 5)
        (y 10)]
       (+ x y)))

(print-line (+ 3 4))
