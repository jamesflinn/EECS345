;Anthony Dario, Anjana Rao, James Flinn
;Interpreter Project

(load "simpleParser.scm")

(define interpret
  (lambda (filename)
    (interpret-help (parser filename) initial-state)))
  
(define interpret-help
  (lambda (tree state)
    (cond
      ((null? tree) state)
      ((eq? (identifier tree) 'var) (interpret-help (cdr tree) (MSdeclare (variable tree) (cddar tree) state)))
      ((eq? (identifier tree) '=) (interpret-help (cdr tree) (MSassign (variable tree) (expression-stmt tree) state)))
      ((eq? (identifier tree) 'if) (interpret-help (cdr tree) (if (MVcondition (condition-stmt tree) state (lambda (v) v))
                                                                  (interpret-help (cons (then-stmt tree) '()) state)
                                                                  (if (else? (car tree))
                                                                      (interpret-help (cons (else-stmt tree) '()) state)
                                                                      state))))
      ((eq? (identifier tree) 'return) (MVreturn (return-stmt tree) state (lambda (v) v)))
      (else (error 'bad-identifier)))))

;This is gonna return the value of an expression
(define MVexpression
  (lambda (expression state return)
       (cond
         ((number? expression) (return expression))
         ((variable? expression) (return (MVvariable expression state)))
         ((eq? '+ (operator expression))(MVexpression (leftoperand expression) state (lambda (v1) (MVexpression (rightoperand expression) state (lambda (v2) (return (+ v1 v2)))))))
         ((eq? '- (operator expression)) 
               (cond
                 ((unary? expression) (MVexpression (leftoperand expression) state (lambda (v) (return (* v -1)))))
                 (else (MVexpression (leftoperand expression) state (lambda (v1) 
                          (MVexpression (rightoperand expression) state (lambda (v2) (return (- v1 v2)))))))))
         ((eq? '* (operator expression)) (MVexpression (leftoperand expression) state (lambda (v1)
                                            (MVexpression (rightoperand expression) state (lambda (v2) (return (* v1 v2)))))))
         ((eq? '/ (operator expression)) (MVexpression (leftoperand expression) state (lambda (v1)
                                                   (MVexpression (rightoperand expression) state (lambda (v2)(return (quotient v1 v2)))))))
         ((eq? '% (operator expression)) (MVexpression (leftoperand expression) state (lambda (v1)
                                                    (MVexpression (rightoperand expression) state (lambda (v2) (return (remainder v1 v2)))))))
        (else (return (MVcondition expression state return)))
       )))

;This should return the value of a condition
(define MVcondition
  (lambda (condition state return)
    (cond
      ((number? condition) (return condition))
      ((variable? condition) (return (MVvariable condition state)))
      ((eq? 'true condition ) (return #t))
      ((eq? 'false condition ) (return #f))
      ((eq? '! (operator condition)) (MVcondition (leftoperand condition) state (lambda (v) (return (not v1)))))
      ((eq? '> (operator condition)) (MVexpression (leftoperand condition) state (lambda (v1) (MVexpression (rightoperand condition) state (lambda (v2) (return (> v1 v2)))))))
      ((eq? '>= (operator condition)) (MVexpression (leftoperand condition) state (lambda (v1) (MVexpression (rightoperand condition) state (lambda (v2) (return (>= v1 v2)))))))
      ((eq? '< (operator condition)) (MVexpression (leftoperand condition) state (lambda (v1) (MVexpression (rightoperand condition) state (lambda (v2) (return (< v1 v2)))))))
      ((eq? '<= (operator condition)) (MVexpression (leftoperand condition) state (lambda (v1) (MVexpression (rightoperand condition) state (lambda (v2) (return (<= v1 v2)))))))
      ((eq? '== (operator condition)) (MVexpression (leftoperand condition) state (lambda (v1) (MVexpression (rightoperand condition) state (lambda (v2) (return (eq? v1 v2)))))))
      ((eq? '!= (operator condition)) (MVexpression (leftoperand condition) state (lambda (v1) (MVexpression (rightoperand condition) state (lambda (v2) (return (not (eq? v1 v2))))))))
      ((eq? '&& (operator condition)) (MVcondition (leftoperand condition) state (lambda (v1) (MVcondition (rightoperand condition) state (lambda (v2) (and v1 v2))))))
      ((eq? '|| (operator condition)) (MVcondition (leftoperand condition) state (lambda (v1) (MVcondition (rightoperand condition) state (lambda (v2) (or v1 v2))))))
      (else (error condition)))))

;This should return the value of a return statement
(define MVreturn
  (lambda (expression state return) 
    (cond
      ((eq? (MVexpression expression state return) #t) (return true))
      ((eq? (MVexpression expression state return) #f) (return false))
      (else (MVexpression expression state return)))))

;this should return the value of a variable
(define MVvariable
  (lambda (variable state)
    (cond
      ((eq? variable 'true) #t)
      ((eq? variable 'false) #f)
      ((null? (namelist state)) (error 'undeclared-variable))
      ((eq? (car (namelist state)) variable) (car (valuelist state)))
      (else (MVvariable variable (cons (cdr (namelist state)) (cons (cdr (valuelist state)) '())))))))
                                             
;this updates the state after a declaration
(define MSdeclare
  (lambda (variable expression state)
    (cond
      ((declared? variable (namelist state)) (error 'redefining))
      ((null? expression) (cons (cons variable (namelist state)) (cons (cons 'error (valuelist state)) '())))
      (else (MSassign variable (car expression) (MSdeclare variable '() state))))))

;this updates the state after an assignment
;trouble with the else statment
(define MSassign
  (lambda (variable expression state)
    (cond
      ((null? (namelist state)) (error 'undeclared-variable))
      ((eq? (car (namelist state)) variable) (cons (namelist state) (cons (cons (MVexpression expression state (lambda (v) v)) (cdr (valuelist state))) '() )))
      (else (cons (cons 
                   (car (namelist state)) 
                   (namelist (MSassign variable expression (append 
                                                            (cons (cdr (namelist state))  '())
                                                            (cons (cdr (valuelist state)) '())))))
                  (cons (cons 
                         (car (valuelist state)) 
                         (valuelist (MSassign variable expression (append 
                                                                   (cons (cdr (namelist state))  '())
                                                                   (cons (cdr (valuelist state)) '()))))) '())
                                                                       )))))

;provides the state for a while loop
(define MSwhile
  (lambda (condition body return state)
    (cond
      ((MVcondition condition state) (return (MSwhile condition body return (MSblock body state))))
      (else (return state)))))


;ABSTRACTIONS
;the empty state
(define initial-state '(() ()))
; the helper functions to determine where the operator and operands are depending on the 
(define operator car)

(define leftoperand cadr)

(define rightoperand caddr)

;these helper functions grab the variable name list or the value list from the state
(define namelist car)

(define valuelist cadr)

;these identify parse tree elements
(define identifier caar)
(define variable cadar)
(define expression-stmt caddar)
(define condition-stmt cadar)
(define then-stmt caddar)
(define else-stmt 
  (lambda (stmt)
    (cadddr (car stmt))))
(define return-stmt cadar)

;this determines if an expression is unary
(define unary? 
  (lambda (l)
    (cond
      ((null? (cddr l)) #t)
      (else #f))))

;determines if an expression is a variable
(define variable?
  (lambda (var)
    (cond
      ((not (list? var)) #t)
      (else #f))))

;determines if a variable is declared
(define declared?
  (lambda (var varnames)
    (cond
      ((null? varnames) #f)
      ((eq? var (car varnames )) #t)
      (else (declared? var (cdr varnames))))))

; return true if if stmt has an else
(define else?
  (lambda (stmt)
    (cond
      ((null? (cdddr stmt)) #f)
      (else #t))))

(interpret "test1.txt")
(interpret "test2.txt")
(interpret "test3.txt")
(interpret "test4.txt")
(interpret "test5.txt")
(interpret "test6.txt")
(interpret "test7.txt")
(interpret "test8.txt")
(interpret "test9.txt")
(interpret "test10.txt")
;(interpret "test11.txt")
;(interpret "test12.txt")
;(interpret "test13.txt")
;(interpret "test14.txt")
(interpret "test15.txt")
;(interpret "test16.txt")
;(interpret "test17.txt")
;(interpret "test18.txt")


