# wisp-newlisp
Wisp to newlisp transpiler 


Transpiles from wisp syntax to newlisp syntax.
Origin of wisp syntax (whitespace to lisp for guile):  https://www.draketo.de/software/wisp


example 
```
define : factorial n 
  if : zero? n
    . 1
    * n : factorial : - n 1
```
transpiles to:
```
  (define (factorial n)
    (if (zero? n)
      1
      (* n (factorial (- n 1)))))
 ```

So instead of left parenthesis use colon  ':' (surrounded by spaces).

Implicitly every line starts with a left parenthesis, if you do not want 
that use a dot '.' (surrounded by spaces) like in the example above.

Right parenthesis are automatically added, based on indentation.
Indent with spaces, tabs are not allowed.(most editors can automatically convert tabs to spaces).

Before and after every function definition put an empty line, 
start every function definition on line position 0 and don't put any 
empty lines within a function definition.

You can add inline comments like in newlisp, but whole line comments 
should start at position 0 and are not allowed within a function. You can stil use parenthesis inline 
but they should balanced per line. 

You can also use dots (.) inline as 
left parens but always in combination with colons (:)

Multi line strings/quoted expressions are not (yet) supported, put them on one line.
