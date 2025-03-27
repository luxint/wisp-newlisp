;; @module indent.lsp
;; @description transpiles from wisp syntax to newlisp syntax.
;; @description origin of wisp syntax (whitespace to lisp) for guile:  https://www.draketo.de/software/wisp
;; @author Ferry de Bruin 2025
;; @version 1.0
;; @syntax  (indent:transpile str-file-name-from str-filename-to)
;; @param <str-file-name-from> filename with the file in wisp format
;; @param <str-filename-to> filename where transpiled file should be saved, will overwrite or create file

;; @example 
;; 
;; define : factorial n 
;;  if : zero? n
;;    . 1
;;    * n : factorial : - n 1
;;
;;  transpiles to:
;;
;;  (define (factorial n)
;;    (if (zero? n)
;;      1
;;      (* n (factorial (- n 1)))))
;; 
;; So instead of left parenthesis use ':' (surrounded by spaces).

;; Implicitly every line starts with a left parenthesis, if you do not want 
;; that use a dot '.' (surrounded by spaces) like in the example above.

;; Right parenthesis are automatically added, based on indentation.
;; Indent with spaces, tabs are not allowed.(most editors can automatically convert tabs to spaces).

;; Before and after every function definition put an empty line, 
;; start every function definition on line position 0 and don't put any 
;; empty lines within a function definition.

;; You can add inline comments like in newlisp, but whole line comments 
;; should start at position 0 and are not allowed within a function, 
;; see below example. You can stil use parenthesis inline 
;; but they should balanced per line. 

;; You can also use dots (.) inline as 
;; left parens but always in combination with colons (:)
;; Multi line strings/quoted expressions are not (yet) supported, put them on one line.
;;
;; ;whole line comment, always start at 0
;; define : fuse lst from to (comp =)
;;  let : found nil result '()
;;    dolist : x lst ; inline comment1
;;      cond  # inline comment2
;;        : and (not found) : comp from x
;;          push x result -1
;;          setq found true
;;        : and found : comp to x
;;          extend (last result) x
;;          setq found nil
;;        found : extend (last result) x
;;        true : push x result -1
;;    when : and found : not : nil? to
;;      throw-error : string lst "not closed!"
;;    . result
;;
;; transpiles to:
;;
;; ; whole line comment, always start at 0
;; (define (fuse lst from to (comp =))
;;   (let (found nil result '())
;;     (dolist (x lst ); inline comment1
;;       (cond  # inline comment2
;;         ((and (not found) (comp from x))
;;           (push x result -1)
;;           (setq found true))
;;         ((and found (comp to x))
;;           (extend (last result) x)
;;           (setq found nil))
;;         (found (extend (last result) x))
;;         (true (push x result -1))))
;;    (when (and found (not (nil? to)))
;;      (throw-error (string lst "not closed!")))
;;    result))


(context 'indent)

;;
;;@syntax (transpile str-file-name-from str-filename-to)
;;@param <str-file-name-from> filename with the file in wisp format
;;@param <str-filename-to> filename where transpiled file should be saved, will overwrite or create file
(define (transpile from to)
  (write-file to (to-nl (read-file from))))

(define (to-nl src , (result '()) (stack '()))
  (setq src (parse-txt src))
  (dolist (line src)
    (cond
      ((< (length line) 2)
        (push "" result -1))
      (true
        (destruct '(comment line) (get-comment line))
        (setq lvl (pop line))
        (when (= 0 lvl)
          (setq stack '())
          (setq balance 0))
        (setq next (if (< $idx (-- (length src)))
                       (first (src (+ 1 $idx)))
                        0))
        (destruct '(bal ln) (make-line line lvl))
        (cond 
          ((> next lvl)
            (push (list lvl balance) stack)
            (setq rp (- bal 1))
            (inc balance))    
          ((= next lvl)
            (setq rp bal))
          (true 
            (setq pb (drop stack next))
            (setq rp (+ bal (- balance pb)))
            (setq balance pb)))
        (if (< rp 0)
          (throw-error (string "unbalanced line " ln)))
        (push (append ln (dup ")" rp) comment) result -1))))
  (join result "\n"))

(define (parse-txt *txt* , (result '()) (line '()))
  (setq compiled (map regex-comp '({^".*"} "^[;|#].*" "^: " "^. ")))
  (setq ind (get-ind))
  (pop *txt* 0 ind)
  (while (not (empty? *txt*))
    (setq res (or (dolist (pat compiled (regex pat *txt* 0x10000)))
                  (list (*txt* 0) 0 1)))
    (if (= (res 0) "{")
      (setq res (get-bstr)))  ; {} bracketed string can be balanced inside, no easy regex so special function
    (cond 
      ((= (res 0) "\n")
        (push (cons ind line) result -1)
        (setq line '())
        (pop *txt*)
        (setq ind (get-ind))
        (pop *txt* 0 ind))
      (true
        (push (res 0) line -1)
        (pop *txt* 0 (res 2)))))
  (push (cons ind line) result -1)
  result)

(setq ind-pat (regex-comp {^[ |\t][ |\t]*} ))

(define (get-ind)
  (let (ind (regex ind-pat *txt* 0x10000))
    (when (and ind (find "\t" (ind 0))) 
      (throw-error "Tabs are not supported, use spaces."))
    (if ind (ind 2) 0)))



(define (get-bstr , (bb 0))
  (setq idx (catch (dostring (x *txt*)
              (case x
                (123 (inc bb))
                (125 (dec bb)))
                (if (zero? bb)
                  (throw (+ 1 $idx))))))
   (if idx
    (list (0 idx *txt*) 0 idx)
    (throw-error "unbalanced brackets in {} string"))   
  )

(define (make-line line lvl)
  (setq st "(")
  (setq dots 0)
  (when (= (line 0) ". ")
    (pop line)
    (setq st "")
    (inc dots))
  (replace ". " line ")" )
  (inc dots $count)
  (replace ": " line "(" )
  (list (- (+ 1 $count) dots) (string (dup " " lvl) st (join line ))))

(define (destruct lst1 lst2)
  (bind (map list lst1 lst2)))

(define (get-comment line , (comment ""))
  (when (regex "^;|^#" (last line))
    (setq comment (pop line -1))
    (if (= (length line) 1)
      (push ". " line -1)))
  (list comment line))

(define (drop stack lvl)
  (if (not stack)
    (throw-error "incorrect indentation"))
  (let (top (first stack))
    (cond 
      ((= (top 0) lvl)
      (top 1))
      ((> (top 0) lvl)
        (pop stack)
        (drop stack lvl))
      (true
        (throw-error "incorrect indentation")))))



(context MAIN)
