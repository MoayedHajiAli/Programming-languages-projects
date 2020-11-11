#lang eopl

;; data structures for let-lang.

(provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (char-val
   (char char?))) 

;;; extractors:

;; expval->num : ExpVal -> Int
;; Page: 70
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

;; expval->bool : ExpVal -> Bool
;; Page: 70
(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval->char
  (lambda (v)
    (cases expval v
      (char-val (char) char)
      (else (expval-extractor-error 'char v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))


;;;;;;;;;;;;;;;;;::Binary tree;;;;;;;;;;;;;;;;;;;;;;;


;; binary tree data structure

(define-datatype btree btree?
  (b-parent
   (left btree?)
   (right btree?)
   (len number?))
  (b-leaf
   (chars list?)
   (len number?)))


;; binary tree observers
(define btree->left
  (lambda (v)
    (cases btree v
      (b-parent (left right len) left)
      (else (eopl:error "node is not a parent")))))

(define btree->right
  (lambda (v)
    (cases btree v
      (b-parent (left right len) right)
      (else (eopl:error "node is not a parent")))))

(define btree->len
  (lambda (v)
    (cases btree v
      (b-parent (left right len) len)
      (b-leaf (rope len) len)
      (else (eopl:error "is not a node")))))

(define btree->chars
  (lambda (v)
    (cases btree v
      (b-leaf (chars len) chars)
      (else (eopl:error "node is not a leaf")))))

;;binary tree predicates
(define (b-leaf? v)
  (cases btree v
    (b-leaf (chars len) #t)
    (else #f)))

(define (b-parent? v)
  (cases btree v
    (b-parent (left right len) #t)
    (else #f)))


;;tests
;(display (b-leaf? (b-leaf '(1, 2, 3) 3)))
;(newline)
;(display (b-parent? (b-leaf '(1, 2, 3) 3)))




;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;; example of a data type built without define-datatype

(define empty-env-record
  (lambda () 
    '()))

(define extended-env-record
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define empty-env-record? null?)

(define environment?
  (lambda (x)
    (or (empty-env-record? x)
        (and (pair? x)
             (symbol? (car (car x)))
             (expval? (cadr (car x)))
             (environment? (cdr x))))))

(define extended-env-record->sym
  (lambda (r)
    (car (car r))))

(define extended-env-record->val
  (lambda (r)
    (cadr (car r))))

(define extended-env-record->old-env
  (lambda (r)
    (cdr r)))
