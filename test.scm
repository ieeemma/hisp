(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(print-line (factorial 7))


(load "lib/list.lsp")

(define my-list (filter id '(3 4 5)))
