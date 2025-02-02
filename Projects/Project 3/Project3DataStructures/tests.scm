(module tests mzscheme
  
  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
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

      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
      (apply-simple-proc "let f = proc (x) -(x,1) in (f 30)" 29)
      (let-to-proc-1 "(proc(f)(f 30)  proc(x)-(x,1))" 29)


      (nested-procs "((proc (x) proc (y) -(x,y)  5) 6)" -1)
      (nested-procs2 "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)"
        -1)
      
       (y-combinator-1 "
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)" 12)
      
       ;; simple letrecs
      (simple-letrec-1 "letrec f(x) = -(x,1) in (f 33)" 32)
      (simple-letrec-2
        "letrec f(x) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)"
        8)

      (simple-letrec-3
        "let m = -5 
 in letrec f(x) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)"
        20)
      
;      (fact-of-6  "letrec
;  fact(x) = if zero?(x) then 1 else *(x, (fact sub1(x)))
;in (fact 6)" 
;                  720)
      
      (HO-nested-letrecs
"letrec even(odd)  = proc(x) if zero?(x) then 1 else (odd -(x,1))
   in letrec  odd(x)  = if zero?(x) then 0 else ((even odd) -(x,1))
   in (odd 13)" 1)

      
      (begin-test-1
        "begin 1; 2; 3 end"
        3)

      (gensym-test-1 
"let g = let counter = newref(0) 
         in proc (dummy) let d = setref(counter, -(deref(counter),-1))
                    in deref(counter)
in -((g 11),(g 22))"
       -1)

      (simple-store-test-1 "let x = newref(17) in deref(x)" 17)

      (assignment-test-1 "let x = newref(17) 
                          in begin setref(x,27); deref(x) end"
        27)

      (gensym-test-2 
"let g = let counter = newref(0) 
         in proc (dummy) begin
                           setref(counter, -(deref(counter),-1));
                           deref(counter)
                         end
 in -((g 11),(g 22))"
       -1)

     (even-odd-via-set-1 "
let x = newref(0)
in letrec even(d) = if zero?(deref(x)) 
                   then 1
                   else let d = setref(x, -(deref(x),1))
                        in (odd d)
          odd(d)  = if zero?(deref(x)) 
                   then 0
                   else let d = setref(x, -(deref(x),1))
                        in (even d)
   in let d = setref(x,13) in (odd -100)" 1)

 (even-odd-via-set-1 "
let x = newref(0)
in letrec even(d) = if zero?(deref(x)) 
                   then 1
                   else let d = setref(x, -(deref(x),1))
                        in (odd d)
          odd(d)  = if zero?(deref(x)) 
                   then 0
                   else let d = setref(x, -(deref(x),1))
                        in (even d)
   in let d = setref(x,13) in (odd -100)" 1)

 (show-allocation-1 "
let x = newref(22)
in let f = proc (z) let zz = newref(-(z,deref(x))) in deref(zz)
   in -((f 66), (f 55))"
   11)

 (chains-1 "
let x = newref(newref(0))
in begin 
    setref(deref(x), 11);
    deref(deref(x))
   end"
   11)

      ; ==================== Array test cases =========================


      (array-detailed-test-1 "let a = newarray(2, -99) in
                              let p = proc (x)
                                  let v = read-array(x, 1)
                                  in update-array(x, 1, -(v, -1))
                       in begin update-array(a, 1, 0); (p a); (p a); read-array(a, 1) end"
                      2)

      (array-detailed-test-2 "let a = newarray(3, 5) in
                              let p = proc (x)
                                   let v = read-array(x, 1)
                                   in update-array(x, 1, -(-2, v))
                              in let q = proc(x)
                                  let v1 = read-array(x, 1) in 
                                  let v2 = read-array(x, 2)    
                                  in update-array(x, 1, -(v2, -(0, v1)))
                       in begin update-array(a, 1, -5); (p a); (q a); read-array(a, 1) end"
                      8)

      (array-detailed-test-3 "let a = newarray(2, -99) in
                              let p = proc (x)
                                  let v = read-array(x, 1)
                                  in update-array(v, 1, -(read-array(v, 2), -(-1, read-array(v, 1))))
                       in begin update-array(a, 1, newarray(3,4)); (p a); (p a); (p a); read-array(read-array(a, 1), 1) end"
                      19)
      

;            ; ==================== Stack test cases =========================;


      ;; aditional test cases
      (stack-test0 "let x = newstack() in begin stack-push(x, 10); stack-push(x, 20); stack-push(x,30); stack-pop(x) ; stack-pop(x) ; stack-top(x) end" 10)
      (stack-test01 "let x = newstack() in begin stack-push(x, 10); stack-push(x, 20); stack-push(x,30); stack-pop(x) ; stack-pop(x); empty-stack?(x) end" #f)
      (stack-test02 "let x = newstack() in begin stack-push(x, 10); stack-push(x, 20); stack-push(x,30); stack-push(x, 40); stack-size(x) end" 4)
      (stack-test03 "let x = newstack() in begin stack-push(x, 10); stack-push(x, 20); stack-push(x,30); stack-push(x, 40); print-stack(x) end" 23) ;; 23 is the dummy value
      (stack-test04 "let x = newstack() in begin stack-push(x, 10); stack-push(x, 20); stack-push(x,30); stack-pop(x) ; stack-pop(x) end" 20)
      
      (stack-test1 "let x = newstack() in begin stack-push(x, 10); stack-push(x, 20); stack-push(x,30); stack-size(x) end" 3)
      (stack-test2 "let x = newstack() in begin stack-push(x, 10); stack-push(x, 20); stack-pop(x); stack-pop(x); stack-push(x, 30); stack-top(x) end" 30)
      (stack-test3 "let x = newstack() in begin stack-push(x, 10); stack-push(x, 20); stack-push(x,30); stack-pop(x); stack-pop(x); stack-pop(x); empty-stack?(x) end" #t)
      (stack-test4 "let x = newstack() in begin stack-push(x, 10); stack-pop(x); stack-push(x, 20); stack-push(x, 30); stack-pop(x); stack-top(x) end" 20)
      (stack-test5 "let x = newstack() in begin stack-push(x, 10); stack-pop(x); stack-push(x, 20); stack-push(x, 30); stack-pop(x); stack-top(x); stack-push(x, 30); stack-size(x) end" 2)
;
;            ; ==================== Queue test cases =========================;

      ;; aditional test cases
      (queue-test00 "let x = newqueue() in begin queue-push(x, 10); queue-push(x, 20); queue-push(x,30); print-queue(x) end" 23) ;; 23 is the dummy value
      (queue-test01 "let x = newqueue() in begin queue-push(x, 10); queue-push(x, 20); queue-push(x,30); queue-top(x) end" 10)
      (queue-test02 "let x = newqueue() in begin queue-push(x, 10); queue-push(x, 20); queue-push(x,30); queue-pop(x) ;queue-top(x) end" 20)
      (queue-test03 "let x = newqueue() in begin queue-push(x, 10); queue-push(x, 20); queue-push(x,30); queue-pop(x) ; queue-pop(x); queue-pop(x); empty-queue?(x) end" #t)
      (queue-test01 "let x = newqueue() in begin queue-push(x, 10); queue-push(x, 20); queue-push(x,30); queue-pop(x) end" 10)
      
      (queue-test1 "let x = newqueue() in begin queue-push(x, 10); queue-push(x, 20); queue-push(x,30); queue-size(x) end" 3)
      (queue-test2 "let x = newqueue() in begin queue-push(x, 10); queue-push(x, 20); queue-push(x, 30); queue-pop(x); queue-pop(x); queue-top(x) end" 30)
      (queue-test3 "let x = newqueue() in begin queue-push(x, 10); queue-push(x, 20); queue-push(x,30); queue-pop(x); queue-pop(x); queue-pop(x); empty-queue?(x) end" #t)
      (queue-test4 "let x = newqueue() in begin queue-push(x, 10); queue-pop(x); queue-push(x, 20); queue-push(x, 30); queue-pop(x); queue-top(x) end" 30)
      (queue-test5 "let x = newqueue() in begin queue-push(x, 10); queue-pop(x); queue-push(x, 20); queue-push(x, 30); queue-pop(x); queue-top(x); queue-push(x, 30); queue-size(x) end" 2)
;      
      ))
  )
