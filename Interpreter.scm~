;Anthony Dario, Anjana Rao, James Flinn
;Interpreter Project

;This is gonna return the value of an expression
(define MVexpression
  (lambda (expression state)
       (cond
         ((number? expression) expression)
         ((variable? expression) (MVvariable expression state))
         ((eq? '+ (operator expression)) (+ (MVexpression (leftoperand expression) state)
                                            (MVexpression (rightoperand expression) state)))
         ((eq? '- (operator expression)) 
               (cond
                 ((unary? expression) (* (MVexpression (leftoperand expression) state) -1))
                 (else (- (MVexpression (leftoperand expression) state)
                          (MVexpressin (rightoperand expression) state)))))
         ((eq? '* (operator expression)) (* (MVexpression (leftoperand expression) state)
                                            (MVexpression (rightoperand expression) state)))
         ((eq? '/ (operator expression)) (quotient (MVexpression (leftoperand expression) state)
                                                   (MVexpression (rightoperand expression) state)))
         ((eq? '% (operator expression)) (remainder (MVexpression (leftoperand expression) state)
                                                    (MVexpression (rightoperand expression) state)))
        (else (error 'bad-operator)))
       ))

;This should return the value of a condition
(define MVcondition
  (lambda (condition state)
    (cond
      ((number? condition) condition)
      ((eq? 'true condition ) #t)
      ((eq? 'false condition ) #f)
      ((eq? '! (operator condition)) (not (MVcondition (leftoperand condition) state)))
      ((eq? '> (operator condition)) (> (MVcondition (leftoperand condition) state) (MVcondition (rightoperand condition) state)))
      ((eq? '>= (operator condition)) (>= (MVcondition (leftoperand condition) state) (MVcondition (rightoperand condition) state)))
      ((eq? '< (operator condition)) (< (MVcondition (leftoperand condition) state) (MVcondition (rightoperand condition) state)))
      ((eq? '<= (operator condition)) (<= (MVcondition (leftoperand condition) state) (MVcondition (rightoperand condition) state)))
      ((eq? '== (operator condition)) (eq? (MVcondition (leftoperand condition) state) (MVcondition (rightoperand condition) state)))
      ((eq? '!= (operator condition)) (not (eq? (MVcondition (leftoperand condition) state) (MVcondition (rightoperand condition) state))))
      ((eq? '&& (operator condition)) (and (MVcondition (leftoperand condition) state) (MVcondition (rightoperand condition) state)))
      ((eq? '|| (operator condition)) (or (MVcondition (leftoperand condition) state) (MVcondition (rightoperand condition) state)))
      (else (error 'bad-operator)))))

;This should return the value of a return statement
(define MVreturn
  (lambda (return expression state) (MVexpression expression state)))

;this should return the value of a variable
(define MVvariable
  (lambda (variable state)
    (cond
      ((null? (namelist state)) (error 'undeclared-variable))
      ((null? (valuelist state)) (error 'unassigned-variable))
      ((eq? (car (namelist state)) variable) (car (valuelist state)))
      (else (MVvariable variable (cons (cdr (namelist state)) (cons (cdr (valuelist state)) '())))))))
                                             
;this updates the state after a declaration
(define MSdeclare
  (lambda (var variable state)
    (cons (append (cons variable '()) (namelist state)) (cons '() (valuelist state)))))

;this updates the state after an assignment
;trouble with the else statment
(define MSassign
  (lambda (variable expression state)
    (cond
      ((null? (namelist state)) (error 'undeclared-variable))
      ((eq? (car (namelist state)) variable) (cons (namelist state) (cons (cons (MVexpression expression state) (valuelist state)) '())))
      (else? ))))
    
    
;ABSTRACTIONS
; the helper functions to determine where the operator and operands are depending on the 
(define operator car)

(define leftoperand cadr)

(define rightoperand caddr)

;these helper functions grab the variable name list or the value list from the state
(define namelist car)

(define valuelist cadr)

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