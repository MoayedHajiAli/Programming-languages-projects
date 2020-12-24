(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.


;;;;;;;;;;;;;;;; Helper Methods ;;;;;;;;;;;;;;;;;;;;;;;;;

  (define INIT-SIZE 1010)
  (define EMPTY-VAL -1)
  
  (define (create-array length value)
    (if (= length 0)
        '()
        (cons (newref value) (create-array (- length 1) value)))
    )

  (define (get-first-nonempty arr)
    (define (iterate lst)
      (cond
        ((null? lst) 0)
        ((= -1 (expval->num (deref (car lst)))) (+ 1 (iterate (cdr lst))))
        (else 0)
        )
      )
    (cases arrval arr
      (list-arr (lst) (iterate lst))
      )
    )

  (define (get-last-nonempty arr)

    (define (iterate lst index answer)
      (cond
        ((null? lst) answer)
        ((= EMPTY-VAL (expval->num (deref (car lst))) ) (iterate (cdr lst) (+ index 1) answer))
        (else (iterate (cdr lst) (+ index 1) index))
        )
      )
    (cases arrval arr
      (list-arr (lst) (iterate lst  0 EMPTY-VAL))
      )
    )

  (define (array-print arr)
    (define (iterate lst)
      (cond
        ((null? lst) (newline) (num-val 23))
        ((= EMPTY-VAL (expval->num (deref (car lst)))) (iterate (cdr lst)))
        (else (display (deref (car lst))) (iterate (cdr lst)))))
    (cases arrval arr
      (list-arr (lst) (iterate lst))
      )
    )
  
  (define (get-size arr)  
    (let ((l (get-first-nonempty arr))
          (r (get-last-nonempty arr)))
      (if (= r -1) 0 (- (+ r 1) l))
      )
    )

  (define (update-array arr index value)
    (cases arrval arr
      (list-arr (lst)
                (setref! (list-ref lst index) value)))
    )

  (define (read-array arr index)
    (cases arrval arr
      (list-arr (lst) (deref (list-ref lst index))))
    )

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        (newarr-exp (exp1 exp2)
                    (arr-val (list-arr (create-array (expval->num (value-of exp1 env)) (value-of exp2 env)))))
        
        (updatearr-exp (exp1 exp2 exp3)
                       (let ((array (expval->arr (value-of exp1 env)))
                             (index (expval->num (value-of exp2 env)))
                             (value (value-of exp3 env))
                             )
                         (update-array array index value)
                         )
                       )

        (readarr-exp (exp1 exp2)
                     (let ((array (expval->arr (value-of exp1 env)))
                           (index (expval->num (value-of exp2 env)))
                           )
                       (read-array array index)
                     ))

        (new-stack-exp ()
                       (arr-val (list-arr (create-array INIT-SIZE (num-val EMPTY-VAL))))
                       )
        
        (push-stack-exp (exp1 exp2)
                        (let ((arr (expval->arr (value-of exp1 env)))
                              (val (value-of exp2 env)))
                          (update-array arr (+ 1 (get-last-nonempty arr)) val)
                          (num-val 23)
                          )
                        )
        (pop-stack-exp (exp1)
                       (let ((arr (expval->arr (value-of exp1 env))))
                         (let ((val (read-array arr (get-last-nonempty arr))))
                           (update-array arr (get-last-nonempty arr) (num-val EMPTY-VAL))
                           val
                         )
                       ))


        (top-stack-exp (exp1)
                       (let ((arr (expval->arr (value-of exp1 env))))
                         (let ((val (read-array arr (get-last-nonempty arr))))
                           val
                         )
                       ))
        
        (empty-stack-exp (exp)
                         (let ((size (get-size (expval->arr (value-of exp env)))))
                           (bool-val (eq? size 0))
                           ))

        (size-stack-exp (exp1)
                        (let ((arr (expval->arr (value-of exp1 env))))
                          (num-val (get-size arr))
                          )
                        )

        (print-stack-exp (exp1)
                         (array-print (expval->arr (value-of exp1 env)))
                         )


        
        (new-queue-exp ()
                       (arr-val (list-arr (create-array INIT-SIZE (num-val EMPTY-VAL))))
                       )
        
        (push-queue-exp (exp1 exp2)
                        (let ((arr (expval->arr (value-of exp1 env)))
                              (val (value-of exp2 env)))
                          (update-array arr (+ 1 (get-last-nonempty arr)) val)
                          (num-val 23)
                          )
                        )
        (pop-queue-exp (exp1)
                       (let ((arr (expval->arr (value-of exp1 env))))
                         (let ((val (read-array arr (get-first-nonempty arr))))
                           (update-array arr (get-first-nonempty arr) (num-val EMPTY-VAL))
                           val
                         )
                       ))


        (top-queue-exp (exp1)
                       (let ((arr (expval->arr (value-of exp1 env))))
                         (let ((val (read-array arr (get-first-nonempty arr))))
                           val
                         )
                       ))
        
        (empty-queue-exp (exp)
                         (let ((size (get-size (expval->arr (value-of exp env)))))
                           (bool-val (eq? size 0))
                           ))

        (size-queue-exp (exp1)
                        (let ((arr (expval->arr (value-of exp1 env))))
                          (num-val (get-size arr))
                          )
                        )

        (print-queue-exp (exp1)
                         (array-print (expval->arr (value-of exp1 env)))
                         )
        
        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  
