
; whole line comment
(define (factorial n)
  (if (zero? n)
    1                    ; suppress opening parens with .
    (* n (factorial (- n 1 )))))
; start a new function on position zero
(define (fibo n)
  (if (< n 2 )
    1
    (+ 
      (fibo (- n 1))
      (fibo (- n 2)))))

