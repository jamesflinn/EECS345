;Anthony Dario, Anjana Rao, James Flinn
;Interpreter Project

(load "classParser.scm")

;interprets some code from a file
(define interpret
  (lambda (filename classname)
    (interpret-global (parser filename) initial-state classname)))

;interprets all the classes and puts them in the state
;when finished, runs the main method
(define interpret-global
  (lambda (tree state classname)
    (cond
      ((null? tree) (MVfunction 'main '() (list (class-method-env (MVvariable classname state temp-class temp-instance)) state) (lambda (v) v) (lambda (v) v)
                                (MVvariable classname state temp-class temp-instance) (create-instance classname '())))
      ((eq? (identifier tree) 'class) (interpret-global (cdr tree) (MSclass (class-name tree) (class-parent tree) (class-body tree) state (lambda (v) v) (create-instance classname '())) classname))
      ((else (error "should only be a class"))))))

;interprets all static variable and function declarations and puts them in the state
(define interpret-static
  (lambda (tree field-env function-env state throw class-env instance)
    (cond
      ((null? tree) (list field-env function-env))
      ((eq? (identifier tree) 'static-var) (interpret-static (cdr tree) 
                                                             (MSdeclare (variable tree) (cddar tree) field-env throw class-env instance) 
                                                             function-env 
                                                             (MSdeclare (variable tree) (cddar tree) state throw class-env instance)
                                                             throw 
                                                             class-env 
                                                             instance))
      ((eq? (identifier tree) 'static-function) (interpret-static (cdr tree) 
                                                                  field-env 
                                                                  (MSfunction (function-name tree) (param-list tree) (function-body tree) function-env class-env instance) 
                                                                  (MSfunction (function-name tree) (param-list tree) (function-body tree) state class-env instance) 
                                                                  throw 
                                                                  class-env 
                                                                  instance))
      ((else (error "unidentified identifier" (identifier tree)))))))

;helper function that inteprets a parse tree
(define interpret-help
  (lambda (tree state return break throw class-env instance)
    (cond
      ((or (number? state) (eq? 'true state) (eq? 'false state)) state)
      ((and (null? tree) (declared? 'main (namelist (top-layer state))) (MVfunction 'main '() state return throw class-env instance)))
      ((null? tree) state)
      ((eq? (identifier tree) 'var) (interpret-help (cdr tree) (MSdeclare (variable tree) (cddar tree) state throw class-env instance) return break throw class-env instance))
      ((eq? (identifier tree) '=) (interpret-help (cdr tree) (MSassign-layer (variable tree) (expression-stmt tree) state state throw class-env instance) return break throw class-env instance))
      ((eq? (identifier tree) 'if) (interpret-help (cdr tree)
                                                   (if (MVcondition (condition-stmt tree) state return throw class-env instance)
                                                       (interpret-help (cons (then-stmt tree) '()) state return break throw class-env instance)
                                                       (if (else? (car tree))
                                                           (interpret-help (cons (else-stmt tree) '()) state return break throw class-env instance)
                                                           state))
                                                   return break throw class-env instance))
      ((eq? (identifier tree) 'return) (MVreturn (return-stmt tree) state return throw class-env instance))
      ((eq? (identifier tree) 'begin) (interpret-help (cdr tree) (remove-layer (interpret-help (get-stmt-list tree) (new-layer state) return break throw class-env instance)) return break throw class-env instance))
      ((eq? (identifier tree) 'while) (interpret-help (cdr tree) (MSwhile (while-condition tree) (while-body tree) state return class-env instance) return break throw class-env instance))
      ((eq? (identifier tree) 'continue) state)
      ((eq? (identifier tree) 'break) (break (remove-layer state)))
      ((eq? (identifier tree) 'function) (interpret-help (cdr tree) (MSfunction (function-name tree) (param-list tree) (function-body tree) state class-env instance) return break throw class-env instance))
      ((eq? (identifier tree) 'funcall) (interpret-help (cdr tree) (begin (MVfunction (fun-call-name (car tree)) (fun-call-params (car tree)) state (lambda (v) v) throw class-env instance) state) return break throw class-env instance))
      ((eq? (identifier tree) 'try) (interpret-help (cdr tree) (interpret-help (finally-body tree) 
                                                                               (interpret-help (try-body tree) state (lambda (v) (return (interpret-help (finally-body tree) v return break throw class-env instance))) break 
                                                                               (lambda (v) (interpret-help (catch-body tree) (add-to-state 'e (box v) state) return break throw class-env instance)) class-env instance)
                                                                               return
                                                                               break
                                                                               throw
                                                                               class-env
                                                                               instance)
                                                                               return break throw class-env instance))
      ((eq? (identifier tree) 'throw) (throw (MVexpression (cadar tree) state return throw class-env instance)))
      (else (error "bad-identifier" (identifier tree)))))) 

;This is gonna return the value of an expression
(define MVexpression
  (lambda (expression state return throw class-env instance)
    (cond
      ((number? expression) (return expression))
      ((boolean? expression) (return expression))
      ((eq? 'true expression) (return #t))
      ((eq? 'false expression) (return #f))
      ((variable? expression) (return (MVenv-var expression state class-env instance)))
      ((eq? 'dot (operator expression)) (return (MVdot (caddr expression) state (get-class-dot (cadr expression) state class-env instance) (get-instance-dot (cadr expression) state))))
      ((function? expression) (return (MVfunction (fun-call-name expression) (fun-call-params expression) state (lambda (v) v) throw class-env instance)))
      ((eq? '+ (operator expression)) (MVexpression (leftoperand expression) state (lambda (v1) (MVexpression (rightoperand expression) state (lambda (v2) (return (+ v1 v2))) throw class-env instance)) throw class-env instance))
      ((eq? '- (operator expression)) 
       (cond
         ((unary? expression) (MVexpression (leftoperand expression) state (lambda (v) (return (* v -1))) throw class-env instance))
         (else (MVexpression (leftoperand expression) state (lambda (v1) 
                                                              (MVexpression (rightoperand expression) state (lambda (v2) (return (- v1 v2))) throw class-env instance)) throw class-env instance))))
      ((eq? '* (operator expression)) (MVexpression (leftoperand expression) state (lambda (v1)
                                                                                     (MVexpression (rightoperand expression) state (lambda (v2) (return (* v1 v2))) throw class-env instance)) throw class-env instance))
      ((eq? '/ (operator expression)) (MVexpression (leftoperand expression) state (lambda (v1)
                                                                                     (MVexpression (rightoperand expression) state (lambda (v2)(return (quotient v1 v2))) throw class-env instance)) throw class-env instance))
      ((eq? '% (operator expression)) (MVexpression (leftoperand expression) state (lambda (v1)
                                                                                     (MVexpression (rightoperand expression) state (lambda (v2) (return (remainder v1 v2))) throw class-env instance)) throw class-env instance)) 
      (else (return (MVcondition expression state return throw class-env instance)))
      )))

;This should return the value of a condition
(define MVcondition
  (lambda (condition state return throw class-env instance)
    (cond
      ((number? condition) (return condition))
      ((eq? 'true condition ) (return #t))
      ((eq? 'false condition ) (return #f))
      ((variable? condition) (return (MVenv-var condition state class-env instance)))
      ((eq? '! (operator condition)) (MVcondition (leftoperand condition) state (lambda (v) (return (not v))) throw class-env instance))
      ((eq? '> (operator condition)) (MVexpression (leftoperand condition) state (lambda (v1) (MVexpression (rightoperand condition) state (lambda (v2) (return (> v1 v2))) throw class-env instance)) throw class-env instance))
      ((eq? '>= (operator condition)) (MVexpression (leftoperand condition) state (lambda (v1) (MVexpression (rightoperand condition) state (lambda (v2) (return (>= v1 v2)))throw class-env instance))throw class-env instance))
      ((eq? '< (operator condition)) (MVexpression (leftoperand condition) state (lambda (v1) (MVexpression (rightoperand condition) state (lambda (v2) (return (< v1 v2))) throw class-env instance)) throw class-env instance))
      ((eq? '<= (operator condition)) (MVexpression (leftoperand condition) state (lambda (v1) (MVexpression (rightoperand condition) state (lambda (v2) (return (<= v1 v2))) throw class-env instance)) throw class-env instance))
      ((eq? '== (operator condition)) (MVexpression (leftoperand condition) state (lambda (v1) (MVexpression (rightoperand condition) state (lambda (v2) (return (eq? v1 v2))) throw class-env instance)) throw class-env instance))
      ((eq? '!= (operator condition)) (MVexpression (leftoperand condition) state (lambda (v1) (MVexpression (rightoperand condition) state (lambda (v2) (return (not (eq? v1 v2)))) throw class-env instance)) throw class-env instance))
      ((eq? '&& (operator condition)) (MVexpression (leftoperand condition) state (lambda (v1) (MVexpression (rightoperand condition) state (lambda (v2) (and v1 v2)) throw class-env instance)) throw class-env instance))
      ((eq? '|| (operator condition)) (MVexpression (leftoperand condition) state (lambda (v1) (MVexpression (rightoperand condition) state (lambda (v2) (or v1 v2)) throw class-env instance)) throw class-env instance))
      (else (error "incorrect operator" (operator condition))))))

;This should return the value of a return statement
(define MVreturn
  (lambda (expression state return throw class-env instance)
    ((lambda (result)
       (cond
         ((eq? result #t) (return 'true))
         ((eq? result #f) (return 'false))
         (else result))) (MVexpression expression state return throw class-env instance))))

;this should return the value of a variable
(define MVvariable
  (lambda (variable state class-env instance)
    (cond
      ((eq? variable 'true) #t)
      ((eq? variable 'false) #f)
      ((null? state) (error "undeclared-variable:" variable))
      ((null? (namelist (top-layer state))) (MVvariable variable (remove-layer state) class-env instance))
      ((and (pair? variable) (eq? (car variable) 'dot)) (MVdot (caddr variable) state (get-class-dot (cadr variable) state class-env instance) (get-instance-dot (cadr variable) state))) 
      ((eq? (car (namelist (top-layer state))) variable) (get-var (length (cdr (namelist (top-layer state)))) (valuelist (top-layer state))))
      (else (MVvariable variable (cons (cons (cdr (namelist (top-layer state))) 
                                             (cons (valuelist (top-layer state)) 
                                                   '())) 
                                       (remove-layer state)) class-env instance)))))

(define get-var
  (lambda (index valuelist)
    (cond
      ((zero? index) (unbox (car valuelist)))
      (else (get-var (- index 1) (cdr valuelist))))))

;this updates the state after a declaration
(define MSdeclare
  (lambda (variable expression state throw class-env instance)
    (cond
      ((declared? variable (namelist (top-layer state))) (error "redefining" variable))
      ((null? expression) (cons (cons (cons variable (namelist (top-layer state)))
                                      (list (append (valuelist (top-layer state)) (list (box 'error)))))
                                (remove-layer state)))
      (else (cons (MSassign variable (car expression) (top-layer (MSdeclare variable '() state throw class-env instance)) state throw class-env instance) (remove-layer state))))))

;helper function that deals with the layers
(define MSassign-layer 
  (lambda (variable expression state layers throw class-env instance)
    (cond
      ((null? state) (error "undeclared-variable:" variable))
      ((or (null? (namelist (top-layer state))) (not (declared? variable (namelist (top-layer state))))) (cons (top-layer state) (MSassign-layer variable expression (cdr state) layers throw class-env instance)))
      (else (cons (MSassign variable expression (top-layer state) layers throw class-env instance) (cdr state)))))) ; variable is in layer

;this updates the state after an assignment
(define MSassign
  (lambda (variable expression state layers throw class-env instance)
    (cond
      ((null? (namelist state)) #f)
      ((not (declared? variable (namelist state))) #f)
      ((eq? (car (namelist state)) variable) (append (list (namelist state)) (list (assign-var (length (cdr (namelist state))) expression (valuelist state) layers throw class-env instance))))
      (else ((lambda (assign)
               (cons (cons
                      (car (namelist state))
                      (namelist assign))
                     (cons
                      (valuelist assign) '()))) 
             (MSassign variable expression (append
                                            (cons (cdr (namelist state))  '())
                                            (cons (valuelist state) '())) layers throw class-env instance))))))

; assigns the variable in the valuelist
(define assign-var
  (lambda (index expression valuelist layers throw class-env instance)
    (cond
      ((zero? index) (begin (set-box! (car valuelist) (MVexpression expression layers (lambda (v) v) throw class-env instance)) valuelist))
      (else (cons (car valuelist) (assign-var (- index 1) expression (cdr valuelist) layers throw class-env instance))))))



; this returns the state after all the class is scanned through
; right now only does static functions
(define MSclass
  (lambda (name parent body state throw instance)
    (add-to-state name
                  (box (create-class (get-parent parent)
                                     ; static field list
                                     (list (append (namelist (field-env (interpret-static body initial-state initial-state state throw temp-class instance))) 
                                                   (namelist (get-parent-fields (get-parent parent) state temp-class temp-instance)))
                                           (append (valuelist (get-parent-fields (get-parent parent) state temp-class temp-instance))
                                                   (valuelist (field-env (interpret-static body initial-state initial-state state throw temp-class instance)))))
                                     (list (append (namelist (function-env (interpret-static body initial-state initial-state state throw temp-class instance))) 
                                                   (namelist (get-parent-funcs (get-parent parent) state temp-class temp-instance)))
                                           (append (valuelist (get-parent-funcs (get-parent parent) state temp-class temp-instance))
                                                   (valuelist (function-env (interpret-static body initial-state initial-state state throw temp-class instance)))))
                                     '())) ; instance variable names will go here
                  state)))

(define get-parent
  (lambda (parent)
    (cond
      ((null? parent) '())
      (else (cadr parent)))))

(define get-parent-fields
  (lambda (parent state class-env instance)
    (cond
      ((null? parent) layer)
      (else (class-field-env (MVvariable parent state class-env instance)))))) 

(define get-parent-funcs
  (lambda (parent state class-env instance)
    (cond
      ((null? parent) layer)
      (else (class-method-env (MVvariable parent state class-env instance))))))

; returns a list in the form of (parent static-field-env method-env instance-field-names)
(define create-class
  (lambda (parent-class field-env method-env field-names)
    (list parent-class field-env method-env field-names)))

; checks if variable is in the class, if so, returns the value of the variable, otherwise returns #f
(define MVclass-var
  (lambda (variable class-env instance)
    (cond
      ((declared? variable (namelist (class-field-env class-env))) (MVvariable variable (list (class-field-env class-env)) class-env instance))
      ((declared? variable (class-field-names class-env)) (MVvariable variable (list (list (class-field-names class-env) (instance-field-values instance))) class-env instance))
      (else 'variable-not-found))))

; checks if variable is in the state, if so, returns the value, otherwise, returns MVclass-var
(define MVenv-var
  (lambda (variable state class-env instance)
    (cond
      ((null? state) (MVclass-var variable class-env instance))
      ((declared? variable (namelist (top-layer state))) (MVvariable variable state class-env instance))
      (else (MVenv-var variable (cdr state) class-env instance)))))

; gets the value of a dot expression
(define MVdot
  (lambda (right-side state class-env instance)
    (if (eq? (MVclass-var right-side class-env instance) 'variable-not-found)
        (MVvariable right-side (list (class-method-env class-env)) class-env instance) ; is a function
        (MVclass-var right-side class-env instance)))) ; is a variable

; returns the class of the left side of the dot
; currently only works with static stuff
; for instances, must get class from instance
(define get-class-dot
  (lambda (name state class-env instance)
    (cond
      ((eq? name 'super) (MVvariable (car class-env) state class-env instance))
      (else (MVvariable name state class-env instance)))))

; returns the instance of the left side of the dot
; currently returns null since we are only dealing with static things for now
(define get-instance-dot
  (lambda (name state)
    '()))

;provides the state for a while loop
(define MSwhile
  (lambda (condition body state return class-env instance)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body state return)
                        (cond
                          ((MVcondition condition state return throw class-env instance) (loop condition body (interpret-help (cons body '()) state return break throw class-env instance) return))
                          (else (return state))))))
         (loop condition body state return))))))

;provides the value for a function call
(define MVfunction
  (lambda (name values state return throw class-env instance)
    (cond
      ((eq? name 'main) (return (interpret-help (closure-body (MVvariable name state class-env instance))
                                                (append (closure-state (MVvariable name state class-env instance)) (last-layer state))
                                                return
                                                'error
                                                throw
                                                class-env instance)))
    (else ((lambda (to-be-named) 
             (return (interpret-help (closure-body to-be-named)                   
                                     (addparams (closure-params to-be-named) (evaluate-params values state (lambda (v1) v1) throw class-env instance) (make-closure-state name 
                                                                                                                                                                          (closure-params to-be-named) 
                                                                                                                                                                          (closure-body to-be-named) 
                                                                                                                                                                          (closure-state to-be-named) 
                                                                                                                                                                          class-env instance) throw class-env instance)      
                                     return
                                     'error throw class-env instance))) (get-func-closure name state class-env instance))))))

; returns the function closure using the function name, state, class environment, and instance
(define get-func-closure
  (lambda (name state class-env instance)
    (MVvariable name (list (class-method-env (MVvariable (instance-class-name instance) state class-env instance)) (last-layer state)) class-env instance)))

(define evaluate-params
  (lambda (values state return throw class-env instance)
    (cond
      ((null? values) (return '()))
      (else (evaluate-params (cdr values) state (lambda (v) (return (cons (MVexpression (car values) state (lambda (v1) v1) throw class-env instance) v))) throw class-env instance)))))

;provides the state after a function call
(define MSfunction
  (lambda (name paramlist body state class-env instance)
    (add-to-state name
                  (box (list paramlist body (new-layer state) (instance-class-name instance)))
                  state)))

;used to return the state we will execute the body of a function in 
(define make-closure-state
  (lambda (name paramlist body state class-env instance) 
    (MSfunction name paramlist body state class-env instance)))

;adds parameters to the state
(define addparams
  (lambda (param-names param-values state throw class-env instance)
    (cond
      ((and (null? param-names) (null? param-values)) state)
      ((null? param-names) (error "too many parameters"))
      ((null? param-values) (error "not enough parameters")) 
      (else (addparams (cdr param-names) (cdr param-values) (MSdeclare (car param-names) (cons (car param-values) '()) state throw class-env instance) throw class-env instance)))))

(define create-instance
  (lambda (class-name field-values)
    (list class-name field-values)))

; adds a name and a value to the given state
(define add-to-state
  (lambda (name value state)
    (cons (cons (cons name (namelist (top-layer state)))
                (list (append (valuelist (top-layer state)) (list value))))
          (cdr state))))

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

; these helper functions grab the variable name list or the value list from the state
(define namelist car)
(define valuelist cadr)

; these help when creating classes
(define field-env caar)
(define function-env caadr)

; these grab elements out of the class env
(define class-field-env cadr)
(define class-method-env caddr)
(define class-field-names cadddr)

; these get elements out of the instance env
(define instance-field-values cadr)
(define instance-class-name car)

(define temp-class '(() (()()) (()()) ()))
(define temp-instance '(() ()))

; try catch finally stuff
(define try-body cadar)
(define catch-body
  (lambda (stmt)
    (caddr (caddar stmt))))
(define finally-body 
  (lambda (stmt)
    (cadar (cdddar stmt))))

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
(define class-name cadar)
(define class-parent caddar)
(define class-body
  (lambda (stmt)
    (car (cdddr (car stmt)))))


;function closure stuff
(define closure-params car)
(define closure-body cadr)
(define closure-state caddr)
(define closure-class cadddr)

;function call stuff
(define fun-call-name cadr)
(define fun-call-params cddr)

;removes a layer of the state
(define remove-layer 
  (lambda (state)
    (cond
      ((pair? state) (cdr state))
      (else state))))

;creates a new layer for the state
(define new-layer
  (lambda (state)
    (cons  layer state)))

(define last-layer
  (lambda (state)
    (car (reverse state))))

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
      ((boolean? var) #f)
      ((eq? var 'true) #f)
      ((eq? var 'false) #f)
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

;(interpret "4test1.txt" 'A)
;(interpret "4test2.txt" 'A)
;(interpret "4test3.txt" 'A)
;(interpret "4test4.txt" 'A)
;(interpret "4test5.txt" 'A)