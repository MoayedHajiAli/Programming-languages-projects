#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")
(require rnrs/mutable-pairs-6)

(provide value-of-program value-of rope concat rope-ref)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))



;;;;;;;;;;;;;;;ropes helper functions;;;;;;

;; initially how many chars to put in a single node
(define NODE_LEN 1)

;; Any leaf with less than MX_LEN length of chars will be considered as a short leaf
(define MX_LEN 3)

(define (concat rope1 rope2)
  ;; merge the content of two leafs into one leaf
  (define (merge-leafs leaf1 leaf2)
    (b-leaf (append (btree->chars leaf1) (btree->chars leaf2)) (+ (btree->len leaf1) (btree->len leaf2)) 0))

  ;; check the case of two short leaft to be merged into one leaf
  (define (short_leafs? rope1 rope2)
    (if (and (or (b-leaf? rope1) (b-empty? rope1)) (or (b-leaf? rope2) (b-empty? rope2)) (<= (+ (btree->len rope1) (btree->len rope2)) MX_LEN))
        #t
        #f))
  
  ;; check the case of mergining right child and right argument
  (define (left-case? rope1 rope2)
    (if (and (b-parent? rope1) (b-leaf? (btree->right rope1)) (b-leaf? rope2) (<= (+ (btree->len (btree->right rope1)) (btree->len rope2)) MX_LEN))
        #t
        #f))
  ;; check the case of mergining left child and left argument
  (define (right-case? rope1 rope2)
    (if (and (b-parent? rope2) (b-leaf? (btree->left rope2)) (b-leaf? rope1) (<= (+ (btree->len (btree->left rope2)) (btree->len rope1)) MX_LEN))
        #t
        #f))
  ;; concatenate
  (cond
    ((b-empty? rope1) rope2)
    ((b-empty? rope2) rope1)
    ((short_leafs? rope1 rope2)
     (merge-leafs rope1 rope2))
    ((left-case? rope1 rope2)
     (let ((left-rope (btree->left rope1))
           (right-rope (concat (btree->right rope1) rope2)))
     (b-parent left-rope right-rope (+ (btree->len left-rope) (btree->len right-rope)) (+ (max (btree->depth left-rope) (btree->depth right-rope)) 1))))
    ((right-case? rope1 rope2)
     (let ((left-rope (concat rope1 (btree->left rope2)))
           (right-rope (btree->right rope2)))
     (b-parent left-rope right-rope (+ (btree->len left-rope) (btree->len right-rope)) (+ (max (btree->depth left-rope) (btree->depth right-rope)) 1))))
    (else (b-parent rope1 rope2 (+ (btree->len rope1) (btree->len rope2)) (+ (max (btree->depth rope1) (btree->depth rope2)) 1)))))


(define (rope chars)
  (define (inner cur_lst chars)
    (cond
      ((eqv? chars '()) (b-leaf cur_lst (length cur_lst) 0))
      ((equal? NODE_LEN (length cur_lst))
       (let ((left (b-leaf cur_lst (length cur_lst) 0))
             (right (inner '() chars)))
         (concat left right)))
      (else (inner (append cur_lst (list (car chars))) (cdr chars)))))
  (inner '() chars))


(define (rope-ref node ind)
  (cases btree node
    (b-leaf (chars len depth)
            (list-ref (btree->chars node) ind))
    (b-parent (left right len depth)
              (let ((left-len (btree->len left))
                    (right-len (btree->len right)))
                (if (>= ind left-len)
                    (rope-ref right (- ind left-len))
                    (rope-ref left ind))))
    (b-empty '())
    ))

(define (substr node start sub-len)
  
  (define (chars-substr lst l r)
    (cond
      ((>= l (length lst)) '())
      ((> l r) '())
      (else (cons (list-ref lst l) (chars-substr lst (+ l 1) r)))
      ))
  
  (define (leaf-substr node l r)
    (let ((chars (chars-substr (btree->chars node) l r 0)))
      (b-leaf chars (length chars))
      ))
  (if (>= start (btree->len node))
      (b-empty)
      (cases btree node
        (b-leaf (chars len depth)
                (if (> (+ start sub-len) len)
                    (b-leaf (chars-substr chars start (- len 1)) (- len start) 0)
                    (b-leaf (chars-substr chars start (+ start (- sub-len 1))) sub-len 0)))
        (b-parent (left right len depth)
                  (let ((left-rope (if (and (= start 0) (>= sub-len (btree->len left))) left (substr left start (min sub-len ( - (btree->len left) 1))))))
                    (let ((right-rope (if (and (< start (btree->len left)) (>= (+ start sub-len) len))
                                          right
                                          (substr right (max (- start (btree->len left)) 0) (- sub-len (btree->len left-rope))))))
                      (concat left-rope right-rope)
                      )))
        (b-empty '())
        )))



(define (fibonacci n)
  (if (< n 2) 1 (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (lower-fib val index)
  (if (<= (fibonacci index) val) index (lower-fib val (+ index 1))))

(define (get-leafs-as-list node)
  (cases btree node
    (b-leaf (chars len depth) (list node))
    (b-parent (left right len depth) (append (get-leafs-as-list left) (get-leafs-as-list right)))
    (b-empty '())))

(define (list-with lst idx val k)
  (cond
    ((null? lst) '())
    ((= k idx) (cons val (cdr lst)))
    (else (cons (car lst) (list-with (cdr lst) idx val (+ k 1))))
    )
  )

;(define l (list 1 2 3 4))
;(display (list-with l 2 10 0))

;(define (list-set! list k val)
 ;   (if (zero? k)
  ;      (set-car! list val)
   ;     (list-set! (cdr list) (- k 1) val)))

(define (rebalance node)

  (define (finalize slots index collected)
    ;(display slots)
    ;(newline)
    ;(display index)
    ;(newline)
    ;(display collected)
    ;(newline)
    ;(newline)
    (cond
      ((= index (length slots)) collected)
      ((b-empty? (list-ref slots index)) (finalize slots (+ index 1) collected))
      (else (finalize slots (+ index 1) (concat (list-ref slots index) collected))))
    )

  (define (attack leaf slots index collected)
    ;(display leaf)
    ;(newline)
    ;(display slots)
    ;(newline)
    ;(display index)
    ;(newline)
    ;(display collected)
    ;(newline)
    ;(newline)
    (let ((fib-val (fibonacci index)))

      (cond
        ((>= index (length slots)) slots)
        ((> fib-val (btree->len leaf))
         (if (b-empty? (list-ref slots index))
             (attack leaf slots (+ index 1) collected)
             (let ((node (list-ref slots index)))
               (begin (set! slots (list-with slots index (b-empty) 0))
                      (attack leaf slots (+ index 1) (concat node collected))
                      ))))
        ((<= fib-val (btree->len leaf))
         
         (let ((new-node (concat collected leaf)))
           (if (b-empty? (list-ref slots index))
               (if (b-empty? collected)
                    (begin  (set! slots (list-with slots index leaf 0)) slots)
                    (attack (concat collected leaf) slots 1 (b-empty)))

                (if (b-empty? collected)
                    (begin (set! slots (list-with slots index (concat node leaf) 0)) slots)
                    (begin (set! slots (list-with slots index (b-empty) 0)) (attack (concat collected (concat node leaf)) slots 1 (b-empty)))
                    )
                )
           )
         )
        )
      )
    )
  
  (define (process leafs slots)
    (cond
      ((null? leafs) (finalize slots 1 (b-empty)))
      (else (set! slots (attack (car leafs) slots 1 (b-empty))) (process (cdr leafs) slots))
      )
    )

  (define (empty-slots size)
    (cond
      ((= size 0) '())
      (else (cons (b-empty) (empty-slots (- size 1)))))
    )

  (let ((leafs (get-leafs-as-list node))
        (slots (empty-slots (+ (btree->depth node) 1))))
    (process leafs slots)
    )
  )


;(define r (rope (list 1 2 3 4 5 6)))
;(display r)
;(newline)
;(display (substr r 1 4))

;(display (get-leafs-as-list r))
;(newline)
;(display (car (get-leafs-as-list r)))
;(define br (rebalance r))
;(newline)
;(display br)

;;test
;(display (rope '(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)))


;#(struct:b-parent
;  #(struct:b-leaf ((1) (,2) ,3) 3)
;  #(struct:b-parent
;    #(struct:b-leaf ((,4) (,5) (,6) ,7) 4)
;    #(struct:b-parent
;      #(struct:b-leaf ((,8) (,9) (,10) ,11) 4)
;      #(struct:b-parent
;        #(struct:b-leaf ((,12) (,13) (,14) ,15) 4)
;        #(struct:b-leaf (,16) 1)
;        5)
;     9)
;   13)
; 16)

;;
;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      ;;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
      (const-exp (num) (num-val num))
      
      ;;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
      (var-exp (var) (apply-env env var))
      
      ;;\commentbox{\diffspec}
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      ;;\commentbox{\zerotestspec}
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      
      ;;\commentbox{\ma{\theifspec}}
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      
      ;;\commentbox{\ma{\theletspecsplit}}
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;;;;;; ropes behaviour implementation
      (rope-const (chars) (rope chars))

      (concat-exp (exp1 exp2)
                (let ((rope1 (value-of exp1 env))
                      (rope2 (value-of exp2 env)))
                  (concat rope1 rope2)))

      (substr-exp (exp1 l r) (substr (value-of exp1 env) l r))

      (rope-ref-exp(exp1 ind)
                   (let ((rope1 (value-of exp1 env)))
                     (rope-ref rope1 ind)))
                      
      )))

