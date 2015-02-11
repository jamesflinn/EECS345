;Anthony Dario, Anjana Rao, James Flinn
;Interpreter Project

;This is gonna return the value of an expression
(define MVexpression
  (lambda (expression state)
       (cond
         ((number? expression) expression)
         ((eq? '+ (operator expression)) (+ (MVexpression (leftoperand expression) state)
                                            (MVexpression (rightoperand expression) state)))
         ((eq? '- (operator expression)) 
               (cond
                 ((unary? expression) (* (MVexpression (leftoperand expression) state) -1))
                 (else (- (MVexpression (leftoperand expression) state)
                          (MVexpressin (rightoperand expression) state)))))
         ((eq? '* (operator expression)) (* (MVexpression (leftoperand expression) state)
                                            (MVexpressin (rightoperand expression) state)))
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

; the helper functions to determine where the operator and operands are depending on the 
(define operator car)

(define leftoperand cadr)

(define rightoperand caddr)

;this determines if an expression is unary
(define unary? 
  (lambda (l)
    (cond
      ((null? (cddr l)) #t)
      (else #f))))