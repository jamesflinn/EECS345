;Anthony Dario, Anjana Rao, James Flinn
;Interpreter Project

;This is gonna return the value of a integer
(define MVexpression
  (lambda (expression state)
       (cond
         ((number? expression) expression)
         ((eq? '+ (operator expression)) (+ (MVexpression (leftoperand expression) state)
                                            (MVexpression (rightoperand expression) state)))
         ((eq? '- (operator expression)) 
               (cond
                 ((null? (cddr expression)) (* (MVexpression (leftoperand expression) state) -1))
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


; the helper functions to determine where the operator and operands are depending on the 
(define operator car)

(define leftoperand cadr)

(define rightoperand caddr)