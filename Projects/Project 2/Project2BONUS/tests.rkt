#lang eopl
(require eopl/tests/private/utils)

(require "data-structures.rkt")  ; for expval constructors
(require "lang.rkt")             ; for scan&parse
(require "interp.rkt")           ; for value-of-program

;; run : String -> ExpVal
;; Page: 71
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define equal-answer?
  (lambda (ans correct-ans)
    (equal? ans (sloppy->expval correct-ans))))

(define sloppy->expval 
  (lambda (sloppy-val)
    (cond
      ((number? sloppy-val) (num-val sloppy-val))
      ((boolean? sloppy-val) (bool-val sloppy-val))
      (else (rope sloppy-val))
            )))

(define-syntax-rule (check-run (name str res) ...)
  (begin
    (cond [(eqv? 'res 'error)
           (check-exn always? (lambda () (run str)))]
          [else
           (check equal-answer? (run str) 'res (symbol->string 'name))])
    ...))

;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
(check-run
 ;; simple arithmetic
 (positive-const "11" 11)
 (negative-const "-33" -33)
 (simple-arith-1 "-(44,33)" 11)
 
 ;; nested arithmetic
 (nested-arith-left "-(-(44,33),22)" -11)
 (nested-arith-right "-(55, -(22,11))" 44)
 
 ;; simple variables
 (test-var-1 "x" 10)
 (test-var-2 "-(x,1)" 9)
 (test-var-3 "-(1,x)" -9)
 
 ;; simple unbound variables
 (test-unbound-var-1 "foo" error)
 (test-unbound-var-2 "-(x,foo)" error)
 
 ;; simple conditionals
 (if-true "if zero?(0) then 3 else 4" 3)
 (if-false "if zero?(1) then 3 else 4" 4)
 
 ;; test dynamic typechecking
 (no-bool-to-diff-1 "-(zero?(0),1)" error)
 (no-bool-to-diff-2 "-(1,zero?(0))" error)
 (no-int-to-if "if 1 then 2 else 3" error)
 
 ;; make sure that the test and both arms get evaluated
 ;; properly. 
 (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
 (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
 
 ;; and make sure the other arm doesn't get evaluated.
 (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
 (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)
 
 ;; simple let
 (simple-let-1 "let x = 3 in x" 3)
 
 ;; make sure the body and rhs get evaluated
 (eval-let-body "let x = 3 in -(x,1)" 2)
 (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)
 
 ;; check nested let and shadowing
 (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
 (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
 (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)
 )

; the ropes is stored in a binary tree where each node is either a leaf or a parant, and each node has a certain depth, and len which represents the length of the ropes in its sub-tree
;  TEST 1 building the rope out of list of chars where MX_LEN is 3
(display (run "rope('h'e'l'l'o'w'o'r'l'd)"))
;#(struct:b-parent
;  #(struct:b-leaf ('h) 1 0)
;  #(struct:b-parent
;    #(struct:b-leaf ('e 'l 'l) 3 0)
;    #(struct:b-parent
;      #(struct:b-leaf ('o 'w 'o) 3 0)
;      #(struct:b-leaf ('r 'l 'd) 3 0)
;    6 1)
;   9 2)
; 10 3)
(newline)

; TEST 2 concatinating two strings
(display (run "concat(rope('h'e'l'l'o), rope('w'o'r'l'd))"))
;#(struct:b-parent
;  #(struct:b-parent
;    #(struct:b-leaf ('h 'e) 2 0)
;    #(struct:b-leaf ('l 'l 'o) 3 0)
;    5 1)
;  #(struct:b-parent
;    #(struct:b-leaf ('w 'o) 2 0)
;    #(struct:b-leaf ('r 'l 'd) 3 0)
;    5 1)
;  10 2)
(newline)
; TEST 3 A more complicated example
(display (run "concat(rope('h'e), concat(rope('l'l'o'w), rope('o'r'l'd)))"))
;#(struct:b-parent
;  #(struct:b-leaf ('h 'e) 2 0)
;  #(struct:b-parent
;    #(struct:b-parent
;      #(struct:b-leaf ('l) 1 0)
;      #(struct:b-leaf ('l 'o 'w) 3 0)
;      4 1)
;    #(struct:b-parent
;      #(struct:b-leaf ('o) 1 0)
;      #(struct:b-leaf ('r 'l 'd) 3 0)
;      4 1)
;    8 2)
;  10 3)
(newline)

; TEST 4 fetching the third char from hello
(display (run "rope-ref(rope('h'e'l'l'o), 2)")) ;; 'l
(newline)

; TEST 5 fething the 5th char from Moayed
(display (run "rope-ref(concat(rope('M'o), concat(rope('a'y), rope('e'd))), 4)")) ;;'e
(newline)

;TEST 6 getting substring of length 6 starting from the second char which is "ellowo"
(display (run "substr(rope('h'e'l'l'o'w'o'r'l'd), 1, 6)"))
(newline)
;#(struct:b-parent
;  #(struct:b-leaf ('e 'l 'l) 3 0)
;  #(struct:b-leaf ('o 'w 'o) 3 0)
;  6 1)

;TEST 7 getting substring of length 4 starting from the third char which is "llow"
;this test case is different from 5 as no nodes can be taken as a whole. Refer to TEST1 for the structure of the helloworld rope
(display (run "substr(rope('h'e'l'l'o'w'o'r'l'd), 2, 4)"))
(newline)
;#(struct:b-parent
;  #(struct:b-leaf ('l 'l) 2 0)
;  #(struct:b-leaf ('o 'w) 2 0)
;  4 1)

;TEST 8 getting substring of length 3 starting from the fourth char which is "low"
;this test case demonstrate how short nodes are concatenated as explained in the paper
(display (run "substr(rope('h'e'l'l'o'w'o'r'l'd), 3, 3)"))
(newline)
;#(struct:b-leaf ('l 'o 'w) 3 0)

;TEST 9 construct rope for a long list of char (abcdefghijk). We can see the construction results in a deep tree (or depth 3) which will be rebalanced in TEST 10
(display (run "rope('a'b'c'd'e'f'g'h'i'j'k)"))
(newline)
;#(struct:b-parent
;  #(struct:b-leaf ('a 'b) 2 0)
;  #(struct:b-parent
;    #(struct:b-leaf ('c 'd 'e) 3 0)
;    #(struct:b-parent
;      #(struct:b-leaf ('f 'g 'h) 3 0)
;      #(struct:b-leaf ('i 'j 'k) 3 0)
;      6 1)
;    9 2)
;  11 3)
; the rebalance operation rebalances the rope according to the rebalance algorithm described in the paper

;TEST 10 rebalancing the rope in TEST 9. we can see that the depth of the tree was reduced by one.
(display (run "rebalance(rope('a'b'c'd'e'f'g'h'i'j'k))"))
;#(struct:b-parent
;  #(struct:b-parent
;    #(struct:b-leaf ('a 'b) 2 0)
;    #(struct:b-leaf ('c 'd 'e) 3 0) 5 1)
;  #(struct:b-parent
;    #(struct:b-leaf ('f 'g 'h) 3 0)
;    #(struct:b-leaf ('i 'j 'k) 3 0)
;    6 1)
;  11 2)
