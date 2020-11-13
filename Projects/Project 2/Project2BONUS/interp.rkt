#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

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
(define NODE_LEN 4)
(define MX_LEN 4)

(define (concat rope1 rope2)
  ;; merge the content of two leafs into one leaf
  (define (merge-leafs leaf1 leaf2)
    (b-leaf (append (btree->chars leaf1) (btree->chars leaf2)) (+ (btree->len leaf1) (btree->len leaf2))))
    
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
    ((left-case? rope1 rope2)
     (b-parent (btree->left rope1) (merge-leafs (btree->right rope1) rope2) (+ (btree->len rope1) (btree->len rope2))))
    ((right-case? rope1 rope2)
     (b-parent (merge-leafs rope1 (btree->left rope2))  (btree->right rope2) (+ (btree->len rope1) (btree->len rope2))))
    (else (b-parent rope1 rope2 (+ (btree->len rope1) (btree->len rope2))))))


(define (rope chars)
  (define (inner cur_lst chars)
    (cond
      ((eqv? chars '()) (b-leaf cur_lst (length cur_lst)))
      ((equal? NODE_LEN (length cur_lst))
       (let ((left (b-leaf cur_lst (length cur_lst)))
             (right (inner '() chars)))
         (concat left right)))
      (else (inner (append cur_lst (list (car chars))) (cdr chars)))))
  (inner '() chars))


(define (rope-ref node ind)
  (cases btree node
    (b-leaf (chars len)
            (list-ref (btree->chars node) ind))
    (b-parent (left right len)
              (let ((left-len (btree->len left))
                    (right-len (btree->len right)))
                (if (>= ind left-len)
                    (rope-ref right (- ind left-len))
                    (rope-ref left ind))))))

(define (substr node start sub-len)
  ;(display node)
  ;(newline)
  ;(display start)
  ;(newline)
  ;(display sub-len)
  ;(newline)
  ;(newline)
  (define (chars-substr lst l r)
   ; (display lst)
   ; (display l)
    ;(display r)
    ;(newline)
    (cond
      ((>= l (length lst)) '())
      ((> l r) '())
      (else (cons (list-ref lst l) (chars-substr lst (+ l 1) r)))
      ))
  
  (define (leaf-substr node l r)
    (let ((chars (chars-substr (btree->chars node) l r 0)))
      (b-leaf chars (length chars))
      ))

  (cases btree node
    (b-leaf (chars len)
            (if (>= (+ start sub-len) len)
                (b-leaf (chars-substr chars start (+ start (- len 1))) len)
                (b-leaf (chars-substr chars start (+ start (- sub-len 1))) sub-len)))
    (b-parent (left right len)

              (let ((left-rope (if (and (= start 0) (>= sub-len (btree->len left))) left (substr left start sub-len))))
                (let ((right-rope (if (and (< start (btree->len left)) (>= (+ start sub-len) len))
                                      right
                                      (substr right (max (- start (btree->len left)) 0) (- sub-len (btree->len left-rope))))))
                  (concat left-rope right-rope)
                  )
                ))
    
    ))

;(define r (rope (list 1 2 3 4 5 6 7 8)))
;(display (substr r 3 7))

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

      (substr-exp (exp1 l r) (substr exp1 l r))

      (rope-ref-exp(exp1 ind)
                   (let ((rope1 (value-of exp1 env)))
                     (rope-ref rope1 ind)))
                      
      )))

