(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    ;;;;;;;;;;;;;;;;;;;;;;; TASK 5 ;;;;;;;;;;;;;;;;;;;;;;;
    ; implement an emptylist value expval
    (emptylist-val)
    ; implement a pair value expval
    (pair-val
      (exp1 expval?)
      (exp2 expval?))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    )

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

  ;;;;;;;;;;;;;;;;;;;;;;; TASK 5 ;;;;;;;;;;;;;;;;;;;;;;;
  ;; implement expval->car
  (define expval->car
    (lambda (v)
      (cases expval v
        (pair-val(exp1 exp2) exp1)
        (else (expval-extractor-error 'car v)))))
  ;; implement expval->cdr
  (define expval->cdr
    (lambda (v)
      (cases expval v
        (pair-val(exp1 exp2) exp2)
        (else (expval-extractor-error 'cdr v)))))
  ;; implement expval->null?
  (define expval->null?
    (lambda (v)
      (cases expval v
	      (emptylist-val() #t)
	      (else #f))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

  ;; Page: 148
  (define identifier? symbol?)

  (define-datatype continuation continuation?
    (end-cont)                 
    (zero1-cont
      (saved-cont continuation?))
    (let-exp-cont
      (var identifier?)
      (body expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (if-test-cont 
      (exp2 expression?)
      (exp3 expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (diff1-cont                
      (exp2 expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (diff2-cont                
      (val1 expval?)
      (saved-cont continuation?))
    (rator-cont            
      (rand expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (rand-cont             
      (val1 expval?)
      (saved-cont continuation?))
    ;;;;;;;;;;;;; TASK 4: LIST ;;;;;;;;;;;;;;;;;;
    ; implement continuation datatype for car expression
    (car-cont            
      (saved-cont continuation?))
    ; implement continuation datatype for cdr expression
    (cdr-cont            
      (saved-cont continuation?))
    ; implement continuation datatype for null? expression
    (null?-cont            
      (saved-cont continuation?))
    ; implement continuation datatype for your list expression
    (list-cont         
      (exps list?)
      (saved-env environment?)
      (saved-cont continuation?))

    (list-saved-cont
      (ent expval?)
      (saved-cont continuation?)
     )
    
    ; implement continuation datatype(s) for your map expression

    (map-cont0
     (lst expression?)
     (saved-env environment?)
      (saved-cont continuation?))


    (map-cont1
     (proc expval?)
     (saved-env environment?)
      (saved-cont continuation?))

    (map-cont2
     (proc expval?)
     (lst expval?)
     (saved-env environment?)
      (saved-cont continuation?))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    )

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))
  
;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec
      (p-name symbol?)
      (b-var symbol?)
      (p-body expression?)
      (saved-env environment?)))

)
