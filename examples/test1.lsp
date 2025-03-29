
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

; transpile newlisp to indent

; if we exclude comments we could use read-expr
; lets add new line after first nested list.


(define (get-offset)
  (let ( offset 0 range (- (length *result*)))
    (unless (empty? *result*)
      (for (x -1 range -1 (= "\n" (*result* x)))
        (inc offset)))
    offset))


(define (read-all-expr file result "")
  (letn (src (read-file file) len (length src))
    (setq expr (read-expr src))
    (do-while (< $count len)
      (extend expr (read-expr src)))
    expr))

(define (emit-expr expr lvl (*result* "")(start true) , (ind 2) )
  (dolist (x expr)
    (cond
      ((list? x)
        (extend *result* 
          (if start ":" "\n")
          (emit-expr (x (+ 1 lvl)   ))
          "."))
      ((string? x)
        (extend result)))))




