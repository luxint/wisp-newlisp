
; whole line comment
(define (factorial n)
  (if (zero? n)
    1                    ; suppress opening parens with .
    (* n (factorial (- n 1 )))))

; start a new function on position zero
(define (fibo n)
  (if (< n 2 )
    1
    (+ (fibo (- n 1))
        (fibo (- n 2)))))

; fast fibonacci generator
(define (fibogen n)
  (let (x 1)
    (series x 
          (fn (y) (+ x (swap y x)))
          n)))

;TODO  transpile newlisp to indent 


(define (get-offset)
  (let ( offset 0 range (- (length *result*)))
    (unless (empty? *result*)
      (for (x -1 range -1 (= "\n" (*result* x)))
        (inc offset)))
    offset))


(define (read-all-expr file (result ""))
  (letn (src (read-file file) len (length src))
    (setq expr (read-expr src))
    ;|# catch 
    ;|#   do-while : < $count len
    ;|#     if : read-expr src 'MAIN 'err $count
    ;|#       extend expr $it
    ;|#       throw expr
    ;|#   . 'err
    expr))

(define (emit-expr expr (lvl 0)  )
  (dolist (x expr)
    (cond
      ((list? x)
        (extend result (string "\n" (dup "  " lvl)))
        (emit-expr x (+ 1 lvl)  ))
      ((string? x)
        (extend result (string { "} x {" })))
      (true
        (extend result (string " " x " ")))))
  result)





