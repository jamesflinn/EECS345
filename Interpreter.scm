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
      ((eq? (identifier tree) 'if) (interpret-help (cdr tree) (if (MVcondition (condition-stmt tree) state)
                                                                  (interpret-help (cons (then-stmt tree) '()) state)
                                                                  (if (else? (car tree))
                                                                      (interpret-help (cons (else-stmt tree) '()) state)
                                                                      state))))
      ((eq? (identifier tree) 'return) (MVreturn (return-stmt tree) state))
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
                 ((unary? expression) (MVexpression (leftoperand expression) state (lambda (v) (* v -1)))
                 (else (MVexpression (leftoperand expression) state (lambda (v1) 
                          (MVexpression (rightoperand expression) state (lambda (v2) (return (-v1 v2))))))))))
         ((eq? '* (operator expression)) (MVexpression (leftoperand expression) state (lambda (v1)
                                            (MVexpression (rightoperand expression) state (lambda (v2) (return (*v1 v2)))))))
         ((eq? '/ (operator expression)) (MVexpression (leftoperand expression) state (lambda (v1)
                                                   (MVexpression (rightoperand expression) state (lambda (v2)(return (quotient v1 v2)))))))
         ((eq? '% (operator expression)) (MVexpression (leftoperand expression) state (lambda (v1)
                                                    (MVexpression (rightoperand expression) state (lambda (v2) (return (% v1 v2)))))))
        (else (return MVcondition expression state))
       )))

;This should return the value of a condition
(define MVcondition
  (lambda (condition state)
    (cond
      ((number? condition) condition)
      ((variable? condition) (MVvariable condition state))
      ((eq? 'true condition ) #t)
      ((eq? 'false condition ) #f)
      ((eq? '! (operator condition)) (not (MVcondition (leftoperand condition) state)))
      ((eq? '> (operator condition)) (> (MVexpression (leftoperand condition) state) (MVexpression (rightoperand condition) state)))
      ((eq? '>= (operator condition)) (>= (MVexpression (leftoperand condition) state) (MVexpression (rightoperand condition) state)))
      ((eq? '< (operator condition)) (< (MVexpression (leftoperand condition) state) (MVexpression (rightoperand condition) state)))
      ((eq? '<= (operator condition)) (<= (MVexpression (leftoperand condition) state) (MVexpression (rightoperand condition) state)))
      ((eq? '== (operator condition)) (eq? (MVexpression (leftoperand condition) state) (MVexpression (rightoperand condition) state)))
      ((eq? '!= (operator condition)) (not (eq? (MVexpression (leftoperand condition) state) (MVexpression (rightoperand condition) state))))
      ((eq? '&& (operator condition)) (and (MVcondition (leftoperand condition) state) (MVcondition (rightoperand condition) state)))
      ((eq? '|| (operator condition)) (or (MVcondition (leftoperand condition) state) (MVcondition (rightoperand condition) state)))
      (else (error condition)))))

;This should return the value of a return statement
(define MVreturn
  (lambda (expression state return) 
    (cond
      ((eq? (MVexpression expression state) #t) (return true))
      ((eq? (MVexpression expression state) #f) (return false))
      (else (MVexpression expression state)))))

;this should return the value of a variable
(define MVvariable
  (lambda (variable state return)
    (cond
      ((eq? variable 'true) (return #t))
      ((eq? variable 'false) (return #f))
      ((null? (namelist state)) (return(error 'undeclared-variable)))
      ((eq? (car (namelist state)) variable) (return (car (valuelist state))))
      (else (MVvariable variable (return (cons (cdr (namelist state)) (cons (cdr (valuelist state)) '()))))))))
                                             
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
      ((eq? (car (namelist state)) variable) (cons (namelist state) (cons (cons (MVexpression expression state) (cdr (valuelist state))) '() )))
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
(interpret "test11.txt")
(interpret "test12.txt")
(interpret "test13.txt")
(interpret "test14.txt")
(interpret "test15.txt")
(interpret "test16.txt")
(interpret "test17.txt")
(interpret "test18.txt")


