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
      ((eq? (identifier tree) 'class) (interpret-global (cdr tree) (MSclass (class-name tree) (class-parent tree) (class-body tree) state (lambda (v) v) (create-instance (class-name tree) '())) classname))
      ((else (error "should only be a class"))))))

;interprets all static variable and function declarations and puts them in the state
(define interpret-static
  (lambda (tree field-env function-env instance-env state throw class-env instance)
    (cond
      ((null? tree) (list field-env function-env instance-env))
      ((eq? (identifier tree) 'static-var) (interpret-static (cdr tree) 
                                                             (MSdeclare (variable tree) (cddar tree) (append field-env (list (last-layer state))) throw class-env instance)
                                                             function-env 
                                                             instance-env
                                                             (MSdeclare (variable tree) (cddar tree) state throw class-env instance)
                                                             throw 
                                                             class-env 
                                                             instance))
      ((eq? (identifier tree) 'var) (interpret-static (cdr tree) 
                                                      field-env
                                                      function-env 
                                                      (MSdeclare (variable tree) (cddar tree) instance-env throw class-env instance)
                                                      (MSdeclare (variable tree) (cddar tree) state throw class-env instance)
                                                      throw
                                                      class-env
                                                      instance))
      ((eq? (identifier tree) 'static-function) (interpret-static (cdr tree) 
                                                                  field-env 
                                                                  (MSfunction (function-name tree) (param-list tree) (function-body tree) function-env #t class-env instance) 
                                                                  instance-env
                                                                  (MSfunction (function-name tree) (param-list tree) (function-body tree) state #t class-env instance) 
                                                                  throw 
                                                                  class-env 
                                                                  instance))
      ((eq? (identifier tree) 'function) (interpret-static (cdr tree)
                                                           field-env
                                                           (MSfunction (function-name tree) (param-list tree) (function-body tree) function-env #f class-env instance)
                                                           instance-env
                                                           (MSfunction (function-name tree) (param-list tree) (function-body tree) state #f class-env instance) 
                                                           throw 
                                                           class-env 
                                                           instance))
      ((else (error "unidentified identifier" (identifier tree)))))))

;helper function that inteprets a parse tree
(define interpret-help
  (lambda (tree state return break throw class-env instance)
    (cond
      ((or (number? state) (eq? 'true state) (eq? 'false state)) state)
      ((and (null? tree) (list? (top-layer state)) (declared? 'main (namelist (top-layer state))) (MVfunction 'main '() state return throw class-env instance)))
      ((null? tree) state)
      ((eq? (identifier tree) 'var) (interpret-help (cdr tree) (MSdeclare (variable tree) (cddar tree) state throw class-env instance) return break throw class-env instance))
      ((eq? (identifier tree) '=) (MSassign-top (variable tree) (expression-stmt tree) state tree return break throw class-env instance))
      ((eq? (identifier tree) 'if) (interpret-help (cdr tree)
                                                   (if (check-condition (MVcondition (condition-stmt tree) state return throw class-env instance))
                                                       (interpret-help (cons (then-stmt tree) '()) state return break throw class-env instance)
                                                       (if (else? (car tree))
                                                           (interpret-help (cons (else-stmt tree) '()) state return break throw class-env instance)
                                                           state))
                                                   return break throw class-env instance))
      ((eq? (identifier tree) 'return) (MVreturn (return-stmt tree) state return throw class-env instance))
      ((eq? (identifier tree) 'begin) (interpret-help (cdr tree) (remove-layer (interpret-help (get-stmt-list tree) (new-layer state) return break throw class-env instance)) return break throw class-env instance))
      ((eq? (identifier tree) 'while) (interpret-help (cdr tree) (MSwhile (while-condition tree) (while-body tree) state return throw class-env instance) return break throw class-env instance))
      ((eq? (identifier tree) 'continue) state)
      ((eq? (identifier tree) 'break) (break (remove-layer state)))
      ((eq? (identifier tree) 'function) (interpret-help (cdr tree) (MSfunction (function-name tree) (param-list tree) (function-body tree) state #f class-env instance) return break throw class-env instance))
      ((eq? (identifier tree) 'funcall) (interpret-help (cdr tree) (begin (MVfunction (fun-call-name (car tree)) (fun-call-params (car tree)) state (lambda (v) v) throw class-env instance) state) return break throw class-env instance))
      ((eq? (identifier tree) 'try) (interpret-help (cdr tree) (MSexception (try-body tree) (catch-body tree) (finally-body tree) (catch-var tree) state return break throw class-env instance) return break throw class-env instance))
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
      ((eq? 'this expression) (return instance))
      ((instance? expression state) (return expression))
      ((variable? expression) (return (if (eq? (MVenv-var expression state class-env instance) 'variable-not-found) (error "variable not found:" expression) (MVenv-var expression state class-env instance))))
      ((eq? 'dot (operator expression)) (return (MVdot (caddr expression) state (get-class-dot (cadr expression) state return throw class-env instance) (get-instance-dot (cadr expression) state return throw class-env instance))))
      ((function? expression) (return (MVfunction (fun-call-name expression) (fun-call-params expression) state (lambda (v) v) throw class-env instance)))
      ((eq? 'new (operator expression)) (MVnew (leftoperand expression) state class-env instance))
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
      ((function? condition) (return (MVfunction (fun-call-name condition) (fun-call-params condition) state (lambda (v) v) throw class-env instance)))
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
      ((and (pair? variable) (eq? (car variable) 'dot)) (MVdot (caddr variable) state (get-class-dot (cadr variable) state (lambda (v) v) (lambda (v) v) class-env instance) (get-instance-dot (cadr variable) state (lambda (v) v) (lambda (v) v) class-env instance))) 
      ((eq? (car (namelist (top-layer state))) variable) (get-var (length (cdr (namelist (top-layer state)))) (valuelist (top-layer state))))
      (else (MVvariable variable (cons (cons (cdr (namelist (top-layer state))) 
                                             (cons (valuelist (top-layer state)) 
                                                   '())) 
                                       (remove-layer state)) class-env instance)))))

; returns the value of the variable given the index of the name in the namelist
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

; helper function that finds where to assign the variable - the state, the static fields, or instance fields
(define MSassign-top
  (lambda (variable expression state tree return break throw class-env instance)
    (cond
      ((and (pair? variable) (eq? 'dot (car variable))) (MSassign-dot variable expression state tree return break throw class-env instance))
      ((var-in-state? variable state) (interpret-help (cdr tree) (MSassign-layer variable expression state state throw class-env instance) return break throw class-env instance))
      ((var-in-static? variable class-env) (interpret-help (cdr tree) state return break throw (create-class (car class-env)
                                                                                                             (MSassign variable 
                                                                                                                       (MVexpression expression state return throw class-env instance) 
                                                                                                                       (class-field-env class-env) (list (class-field-env class-env)) throw class-env instance)
                                                                                                             (class-method-env class-env)
                                                                                                             (class-field-names class-env)
                                                                                                             (class-initial-values class-env)) instance))
      ((var-in-instance? variable class-env) (interpret-help (cdr tree) state return break throw class-env (create-instance (car instance)
                                                                                                                            (valuelist (MSassign variable
                                                                                                                                                 (MVexpression expression state return throw class-env instance)
                                                                                                                                                 (list (class-field-names class-env) (cadr instance)) state throw class-env instance))))) 
      (else (error "undeclared variable" variable)))))

; assigns the variable when a dot expression is involved
(define MSassign-dot
  (lambda (dot-expr expression state tree return break throw class-env instance)
    (cond
      ((eq? 'this (leftside dot-expr)) (interpret-help (cdr tree) state return break throw class-env (create-instance (car instance)
                                                                                                                      (valuelist (MSassign (rightside dot-expr)
                                                                                                                                           (MVexpression expression state return throw class-env instance)
                                                                                                                                           (list (class-field-names class-env) (cadr instance)) state throw class-env instance)))))
                                                     
      ((instance? (MVvariable (leftside dot-expr) state class-env instance) state) 
       (MSassign-top (leftside dot-expr) 
                     (create-instance (instance-class-name (MVvariable (leftside dot-expr) state class-env instance))
                                      (valuelist (MSassign (rightside dot-expr)
                                                   (MVexpression expression state return throw class-env instance)
                                                   (list (class-field-names class-env) (instance-field-values (MVvariable (leftside dot-expr) state class-env instance))) state throw class-env instance)))
                     state tree return break throw class-env instance))
      (else (MSassign-top (rightside dot-expr) expression state tree return break throw
                          (get-class-dot (leftside dot-expr) state return break class-env instance)
                          (get-instance-dot (leftside dot-expr) state return break class-env instance))))))

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
(define MSclass
  (lambda (name parent body state throw instance)
    ((lambda (class-env)
       (add-to-state name
                     (box (create-class (get-parent parent)
                                        (list (append (namelist (field-env class-env)) 
                                                      (namelist (get-parent-fields (get-parent parent) state temp-class temp-instance)))
                                              (append (valuelist (get-parent-fields (get-parent parent) state temp-class temp-instance))
                                                      (valuelist (field-env class-env))))
                                        (list (append (namelist (function-env class-env)) 
                                                      (namelist (get-parent-funcs (get-parent parent) state temp-class temp-instance)))
                                              (append (valuelist (get-parent-funcs (get-parent parent) state temp-class temp-instance))
                                                      (valuelist (function-env class-env))))
                                        (append (namelist (instance-env class-env))
                                                (get-parent-instance-names (get-parent parent) state temp-class temp-instance))
                                        (append (get-parent-initial-values (get-parent parent) state temp-class temp-instance)
                                                (valuelist (instance-env class-env)))))
                     state)) (interpret-static body initial-state initial-state initial-state state throw (create-class (get-parent parent) '() '() '() '())  instance))))

; get the parent, returns null if no parent exists
(define get-parent
  (lambda (parent)
    (cond
      ((null? parent) '())
      (else (cadr parent)))))

; get the fields of the parent
(define get-parent-fields
  (lambda (parent state class-env instance)
    (cond
      ((null? parent) layer)
      (else (class-field-env (MVvariable parent state class-env instance)))))) 

; get the functions of the parent
(define get-parent-funcs
  (lambda (parent state class-env instance)
    (cond
      ((null? parent) layer)
      (else (class-method-env (MVvariable parent state class-env instance))))))

; get the instance name of the parent
(define get-parent-instance-names
  (lambda (parent state class-env instance)
    (cond
      ((null? parent) '())
      (else (class-field-names (MVvariable parent state class-env instance))))))

; get the initial vaues of the parent
(define get-parent-initial-values
  (lambda (parent state class-env instance)
    (cond
      ((null? parent) '())
      (else (class-initial-values (MVvariable parent state class-env instance))))))

; returns a list in the form of (parent static-field-env method-env instance-field-names)
(define create-class
  (lambda (parent-class field-env method-env field-names initial-values)
    (list parent-class field-env method-env field-names initial-values)))

; returns a new instance
(define MVnew
  (lambda (name state class-env instance)
    (create-instance name
                     (create-copy (class-initial-values (MVvariable name state class-env instance))))))

; creates a copy of the initial values of the instance
; used so that each instance has its own boxes of its fields
(define create-copy
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (box (unbox (car l))) (create-copy (cdr l)))))))

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
(define get-class-dot
  (lambda (name state return throw class-env instance)
    (cond
      ((eq? name 'super) (MVvariable (car class-env) state class-env instance))
      ((eq? name 'this) (if (static? instance) (error "no this") (MVvariable (car instance) state class-env instance)))
      ((var-in-instance? name class-env) (MVvariable (instance-class-name (MVvariable name (get-instance-state class-env instance) class-env instance)) state class-env instance))
      ((and (pair? name) (eq? 'funcall (car name))) (MVvariable (instance-class-name (MVexpression name state return throw class-env instance)) state class-env instance)) 
      ((eq? 2 (length (MVvariable name state class-env instance))) (MVvariable (instance-class-name (MVvariable name state class-env instance)) state class-env instance)) ; an instance variable
      (else (MVvariable name state class-env instance)))))

; returns the instance of the left side of the dot
(define get-instance-dot
  (lambda (name state return throw class-env instance)
    (cond
      ((eq? name 'super) instance);(MVnew (car class-env) state (MVvariable (car class-env) state class-env instance) instance))
      ((eq? name 'this) (if (static? instance) (error "no this") instance))
      ((var-in-instance? name class-env) (MVvariable name (get-instance-state class-env instance) class-env instance))
      ((and (pair? name) (eq? 'funcall (car name))) (MVexpression name state return throw class-env instance))
      ((eq? 2 (length (MVvariable name state class-env instance))) (MVvariable name state class-env instance))
      (else (create-instance (car (MVvariable name state class-env instance)) '())))))

;provides the state for a while loop
(define MSwhile
  (lambda (condition body state return throw class-env instance)
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
    (else ((lambda (func-closure) 
             (return (interpret-help (closure-body func-closure)                   
                                     (addparams (closure-params func-closure) (evaluate-params values state (lambda (v1) v1) throw class-env instance) (append (make-closure-state name 
                                                                                                                                                                          (closure-params func-closure) 
                                                                                                                                                                          (closure-body func-closure) 
                                                                                                                                                                          (closure-state func-closure) 
                                                                                                                                                                          (closure-static func-closure)
                                                                                                                                                                          class-env instance) (list (last-layer state))) throw class-env instance)      
                                     return
                                     'error throw (MVvariable (closure-class func-closure) state class-env instance) (get-func-instance name state (closure-static func-closure) return throw class-env instance)))) (get-func-closure name state return throw class-env instance))))))

; returns the function closure using the function name, state, class environment, and instance
(define get-func-closure
  (lambda (name state return throw class-env instance)
    (cond 
      ((and (not (null? (instance-class-name instance))) (var-in-class-method? name (MVvariable (instance-class-name instance) state class-env instance))) 
       (MVvariable name (list (class-method-env (MVvariable (instance-class-name instance) state class-env instance)) (last-layer state)) class-env instance))
      ((var-in-state? name state) (MVvariable name state class-env instance))
      ((and (pair? name) (eq? 'dot (car name))) (MVdot (caddr name) state (get-class-dot (cadr name) state return throw class-env instance) (get-instance-dot (cadr name) state return throw class-env instance)));(MVvariable (caddr name) (list (class-method-env (MVvariable (cadr name) state class-env instance)) (last-layer state)) class-env instance))
      (else (MVvariable name (list (class-method-env (MVvariable (instance-class-name instance) state class-env instance)) (last-layer state)) class-env instance)))))

; evaluate the parameters to be put into the function's state
(define evaluate-params
  (lambda (values state return throw class-env instance)
    (cond
      ((null? values) (return '()))
      (else (evaluate-params (cdr values) state (lambda (v) (return (cons (MVexpression (car values) state (lambda (v1) v1) throw class-env instance) v))) throw class-env instance)))))

; returns the instance of the function
(define get-func-instance
  (lambda (name state is-static return throw class-env instance)
    (cond
      ((and (pair? name) (eq? 'dot (car name))) (get-instance-dot (cadr name) state return throw class-env instance))
      (is-static (append instance (list is-static)))
      (else instance))))

;provides the state after a function call
(define MSfunction
  (lambda (name paramlist body state is-static class-env instance)
    (add-to-state name
                  (box (list paramlist body (new-layer state) (instance-class-name instance) is-static))
                  state)))

;used to return the state we will execute the body of a function in 
(define make-closure-state
  (lambda (name paramlist body state is-static class-env instance) 
    (MSfunction name paramlist body state is-static class-env instance)))

;adds parameters to the state
(define addparams
  (lambda (param-names param-values state throw class-env instance)
    (cond
      ((and (null? param-names) (null? param-values)) state)
      ((null? param-names) (error "too many parameters"))
      ((null? param-values) (error "not enough parameters")) 
      (else (addparams (cdr param-names) (cdr param-values) (MSdeclare (car param-names) (cons (car param-values) '()) state throw class-env instance) throw class-env instance)))))

; creates a new instance
(define create-instance
  (lambda (class-name field-values)
    (list class-name field-values)))

; adds a name and a value to the given state
(define add-to-state
  (lambda (name value state)
    (cons (cons (cons name (namelist (top-layer state)))
                (list (append (valuelist (top-layer state)) (list value))))
          (cdr state))))

; returns the state of the exception
(define MSexception
  (lambda (try-body catch-body finally-body catch-var state return break throw class-env instance)
    ((lambda (try-state)
       (if (list? try-state) 
           (MSfinally finally-body try-state return break throw class-env instance)
           (MVreturn try-state (MSfinally finally-body state return break throw class-env instance) return throw class-env instance))) 
     (MStry try-body catch-body catch-var state return break throw class-env instance))))

; returns the try state
(define MStry
  (lambda (try-body catch-body catch-var state return break throw class-env instance)
    (call/cc
     (lambda (new-throw)
       (remove-layer (interpret-help try-body (new-layer state) return break 
                                     (lambda (v) (new-throw (MScatch catch-body (add-to-state catch-var (box v) state) return break throw class-env instance))) class-env instance))))))

; returns the catch state
(define MScatch
  (lambda (catch-body state return break throw class-env instance)
    (remove-layer (interpret-help catch-body (new-layer state) return break throw class-env instance))))

; returns the finally state
(define MSfinally
  (lambda (finally-body state return break throw class-env instance)
    (remove-layer (interpret-help finally-body (new-layer state) return break throw class-env instance))))
       

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
(define instance-env caaddr)

; these grab elements out of the class env
(define class-field-env cadr)
(define class-method-env caddr)
(define class-field-names cadddr)
(define class-initial-values
  (lambda (stmt)
    (cadddr (cdr stmt))))

; these get elements out of the dot expr
(define leftside cadr)
(define rightside caddr)

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
    (if (null? (car (cdddar stmt))) '()
        (cadar (cdddar stmt)))))
(define catch-var
  (lambda (stmt)
    (caadr (caddar stmt))))

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
(define closure-static
  (lambda (stmt)
    (cadddr (cdr stmt))))

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

; returns the last layer the state
(define last-layer
  (lambda (state)
    (car (reverse state))))

; returns true if the condition is 'true or #t, false otherwise
(define check-condition
  (lambda (condition)
    (cond
      ((or (eq? condition 'true) (eq? condition #t)) #t)
      (else #f))))

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

;determines if an expression is an instance
(define instance?
  (lambda (expr state)
    (and (list? expr) (eq? 2 (length expr)) (declared? (car expr) (namelist (last-layer state))))))

; return true if if stmt has an else
(define else?
  (lambda (stmt)
    (cond
      ((null? (cdddr stmt)) #f)
      (else #t))))

; return true if variable is in the state
(define var-in-state?
  (lambda (variable state)
    (cond
      ((null? state) #f)
      ((declared? variable (namelist (top-layer state))) #t)
      (else (var-in-state? variable (remove-layer state))))))

; return true if variable is in the class method list
(define var-in-class-method?
  (lambda (variable class-env)
    (declared? variable (namelist (class-method-env class-env)))))

; return true if variable is in the static field list
(define var-in-static?
  (lambda (variable class-env)
    (declared? variable (namelist (class-field-env class-env)))))

; return true if variable is in the instance field list
(define var-in-instance?
  (lambda (variable class-env)
    (declared? variable (class-field-names class-env))))

; returns true if the instance is static
(define static?
  (lambda (instance)
    (if (eq? 3 (length instance))
        (caddr instance)
        #f)))

; returns a state that contains the instance names and values
(define get-instance-state
  (lambda (class-env instance)
    (list (list (class-field-names class-env) (instance-field-values instance)))))
      
; used to test examples, returns error if not expected output
(define test
  (lambda (filename classname expected-value)
    (if (eq? (interpret filename classname) expected-value) (list filename 'passed)
        (error "Error:" filename 'expected expected-value 'but 'was 'returned (interpret filename classname)))))

(test "4test1.txt" 'A 10) ; 10
(test "4test2.txt" 'A 'true) ; true
(test "4test3.txt" 'A 30) ; 30
(test "4test4.txt" 'A 'false) ; false
(test "4test5.txt" 'A 30) ; 30
(test "4test5.txt" 'B 510) ; 510
(test "4test6.txt" 'A 30) ; 30
(test "4test6.txt" 'B 530) ; 530
(test "4test7.txt" 'A 105) ; 105
(test "4test7.txt" 'B 1155) ; 1155
(test "4test8.txt" 'B 615) ; 615
;(interpret "4test9.txt" 'B) ; ERROR: variable not found: d
(test "4test9.txt" 'C 4321) ; 4321
(test "4test10.txt" 'Square 400) ; 400
(test "4test11.txt" 'A 15) ; 15
(test "4test12.txt" 'A 125) ; 125
(test "4test13.txt" 'A 100) ; 100
(test "4test14.txt" 'A 2000400)
(test "4test15.txt" 'Pow 64) ; 64

(test "5test1.txt" 'A 20) ; 20
(test "5test2.txt" 'Square 400) ; 400
(test "5test3.txt" 'B 530) ; 530
(test "5test4.txt" 'B 615) ; 615
;(test "5test5.txt" 'C -716) ; -716
(test "5test6.txt" 'A 15) ; 15
(test "5test7.txt" 'A 12) ; 12
(test "5test8.txt" 'A 110) ; 110
(test "5test9.txt" 'A 125) ; 125
(test "5test10.txt" 'A 36) ; 36
(test "5test11.txt" 'A 54) ; 54
(test "5test12.txt" 'C 26)
(test "5test13.txt" 'Square 117)
(test "5test14.txt" 'Square 32)
(test "5test15.txt" 'List 15)
(test "5test16.txt" 'Box 16)
;(test "5test17.txt" 'List 123456)
(test "5test18.txt" 'List 5285)
(test "5test19.txt" 'A 100)
(test "5test20.txt" 'A 420)
(test "5test21.txt" 'A 10)
;(interpret "5test22.txt" 'A) ; ERROR



