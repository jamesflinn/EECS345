;Anthony Dario, Anjana Rao, James Flinn
;Interpreter Project

(load "functionParser.scm")

;interprets some code from a file
(define interpret
  (lambda (filename)
    (interpret-help (parser filename) initial-state (lambda (v) v) (lambda (v) v))))
  
;helper function that inteprets a parse tree
(define interpret-help
  (lambda (tree state return break)
    (cond
      ((and (null? tree) (declared? 'main (namelist (top-layer state))) (MVfunction 'main '() state return)))
      ((null? tree) state)
      ((eq? (identifier tree) 'var) (interpret-help (cdr tree) (MSdeclare (variable tree) (cddar tree) state) return break))
      ((eq? (identifier tree) '=) (interpret-help (cdr tree) (MSassign-layer (variable tree) (expression-stmt tree) state state) return break))
      ((eq? (identifier tree) 'if) (interpret-help (cdr tree) (if (MVcondition (condition-stmt tree) state return)
                                                                  (interpret-help (cons (then-stmt tree) '()) state return break)
                                                                  (if (else? (car tree))
                                                                      (interpret-help (cons (else-stmt tree) '()) state return break)
                                                                      state)) return break))
      ((eq? (identifier tree) 'return) (MVreturn (return-stmt tree) state return))
      ((eq? (identifier tree) 'begin) (interpret-help (cdr tree) (remove-layer (interpret-help (get-stmt-list tree) (new-layer state) return break)) return break))
      ((eq? (identifier tree) 'while) (interpret-help (cdr tree) (MSwhile (while-condition tree) (while-body tree) state return) return break))
      ((eq? (identifier tree) 'continue) state)
      ((eq? (identifier tree) 'break) (break (remove-layer state)))
      ((eq? (identifier tree) 'function) (interpret-help (cdr tree) (MSfunction (function-name tree) (param-list tree) (function-body tree) state return) return break))
      ;((eq? (identifier tree) 'funcall) (interpret-help (cdr tree) (
      (else (error 'bad-identifier)))))

;This is gonna return the value of an expression
(define MVexpression
  (lambda (expression state return)
       (cond
         ((number? expression) (return expression))
         ((variable? expression) (return (MVvariable expression state)))
         ((function? expression) (return (MVfunction (fun-call-name expression) (fun-call-params expression) state return)))
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
      ((eq? '! (operator condition)) (MVcondition (leftoperand condition) state (lambda (v) (return (not v)))))
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
      ((eq? (MVexpression expression state return) #t) (return 'true))
      ((eq? (MVexpression expression state return) #f) (return 'false))
      (else (MVexpression expression state return)))))

;this should return the value of a variable
(define MVvariable
  (lambda (variable state)
    (cond
      ((eq? variable 'true) #t)
      ((eq? variable 'false) #f)
      ((null? state) (error "undeclared-variable:" variable))
      ((null? (namelist (top-layer state))) (MVvariable variable (remove-layer state)))
      ((eq? (car (namelist (top-layer state))) variable) (unbox (car (valuelist (top-layer state)))))
      (else (MVvariable variable (cons (cons (cdr (namelist (top-layer state))) (cons (cdr (valuelist (top-layer state))) '())) (remove-layer state)))))))
                                             
;this updates the state after a declaration
(define MSdeclare
  (lambda (variable expression state)
    (cond
      ((declared? variable (namelist (top-layer state))) (error 'redefining))
      ((null? expression) (cons (cons (cons variable (namelist (top-layer state)))
                                      (cons (cons (box 'error)
                                                  (valuelist (top-layer state)))
                                            '()))
                                (remove-layer state)))
      (else (cons (MSassign variable (car expression) (top-layer (MSdeclare variable '() state)) state) (remove-layer state))))))

;helper function that deals with the layers
(define MSassign-layer
  (lambda (variable expression state layers)
    (cond
      ((null? state) (error 'undeclared-variable))
      ((or (null? (namelist (top-layer state))) (not (declared? variable (namelist (top-layer state))))) (cons (top-layer state) (MSassign-layer variable expression (cdr state) layers)))
      (else (cons (MSassign variable expression (top-layer state) layers) (cdr state)))))) ; variable is in layer

;this updates the state after an assignment
;trouble with the else statment
(define MSassign
  (lambda (variable expression state layers)
    (cond
      ((null? (namelist state)) #f)
      ((not (declared? variable (namelist state))) #f)   ; used to see if the variable is declared in this list, if it isn't go back to MSassign-layer
      ((eq? (car (namelist state)) variable) (cons (namelist state) (cons (cons (begin (set-box! (car (valuelist state)) (MVexpression expression layers (lambda (v) v))) (car (valuelist state))) 
                                                                                (cdr (valuelist state))) '() )))
      (else ((lambda (assign)
               (cons (cons
                      (car (namelist state))
                      (namelist assign))
                     (cons (cons
                            (car (valuelist state))
                            (valuelist assign)) '()))) 
             (MSassign variable expression (append
                                            (cons (cdr (namelist state))  '())
                                            (cons (cdr (valuelist state)) '())) layers))))))
                                                                

;provides the state for a while loop
(define MSwhile
  (lambda (condition body state return)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body state return)
                        (cond
                          ((MVcondition condition state return) (loop condition body (interpret-help (cons body '()) state return break) return))
                          (else (return state))))))
         (loop condition body state return))))))
    
;provides the value for a function call
(define MVfunction
  (lambda (name values state return)
    (interpret-help (closure-body (MVvariable name state))                     ;function body 
                    (addparams (closure-params (MVvariable name state)) values (poop name 
                                                                                     (closure-params (MVvariable name state)) 
                                                                                     (closure-body (MVvariable name state)) 
                                                                                     (closure-state (MVvariable name state)) 
                                                                                     return) 
                               return)           ;function-state 
                    return 
                    'error)))
    
;provides the state after a function call
(define MSfunction
  (lambda (name paramlist body state return)
    (return
     (cons (cons (cons name
                       (namelist (top-layer state)))                                                ;the old namelist
                 (cons (cons (box (list paramlist body (new-layer state)))
                             (valuelist (top-layer state)))
                       '()))                                              ;the old valuelist
           (remove-layer state))                                                                    ;rest of the layers of the state
    )))

;used to return the state we will execute the body of a function in 
;NEEDS A NEW NAME
;NEEDS A NEW NAME
;NEEDS A NEW NAME
;NEEDS A NEW NAME
(define poop
  (lambda (name paramlist body state return) 
    (MSfunction name paramlist body state return)))

;ABSTRACTIONS
;the empty state
(define initial-state '((() ())))

;layer
(define layer '(()()))

(define top-layer car)

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
(define while-condition cadar)
(define while-body caddar)
(define get-stmt-list cdar)
(define function-name cadar)
(define param-list caddar)
(define function-body 
  (lambda (stmt)
    (cadddr (car stmt))))

;function closure stuff
(define closure-params car)
(define closure-body cadr)
(define closure-state caddr)

;function call stuff
(define fun-call-name cadr)
(define fun-call-params cddr)

;removes a layer of the state
(define remove-layer cdr)

;creates a new layer for the state
(define new-layer
  (lambda (state)
    (cons  layer state)))
  
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

;determines if an expression is a function
(define function?
  (lambda (expr)
    (eq? (car expr) 'funcall)))

; return true if if stmt has an else
(define else?
  (lambda (stmt)
    (cond
      ((null? (cdddr stmt)) #f)
      (else #t))))

;adds parameters to the state
(define addparams
  (lambda (param-names param-values state return)
    (cond
      ((null? param-names) (return state))
      ((null? param-values) (error "not enough parameters"))
      (else (addparams (cdr param-names) (cdr param-values) state (lambda (v) (return (MSdeclare (car param-names) (cons (car param-values) '()) v))))))))
                                                              

;(interpret "test1.txt")
;(interpret "test2.txt")
;(interpret "test3.txt")
;(interpret "test4.txt")
;(interpret "test5.txt")
;(interpret "test6.txt")
;(interpret "test7.txt")
;(interpret "test8.txt")
;(interpret "test9.txt")
;(interpret "test10.txt")
;(interpret "test11.txt")
;(interpret "test12.txt")

;(interpret "3test1.txt")
;(interpret "3test2.txt")
;(interpret "3test3.txt")
(interpret "3test4.txt")
(interpret "3test5.txt")
(interpret "3test6.txt")
(interpret "3test7.txt")
(interpret "3test8.txt")
(interpret "3test9.txt")
(interpret "3test10.txt")
(interpret "3test11.txt")
(interpret "3test12.txt")