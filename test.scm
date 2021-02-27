;(define (factorial n)
;  (if (= n 0)
;      1
;      (* n (factorial (- n 1)))))

;(print-line (factorial 7))

(load "lib/list.lsp")

; (print-line `(foo (+ 3 5) ,(+ 3 5)))

; (print-line (zip '(one two three) '(1 2 3) '(odd even odd even odd even odd even)))

; (define (factorial n)
;   (if (zero? n)
;       1
;       (* n (factorial (- n 1)))))
                
; (print-line (factorial 7))

;(define-macro (as-number x)
;(string->number x))

;(define my-number (+ 1 (as-number "hi")))

;(print-line my-number)

(print-line
	(sort '(3 1 7 5 2 9 9 8 1 2)))
