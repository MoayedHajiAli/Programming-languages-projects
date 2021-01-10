(module top (lib "eopl.ss" "eopl")

  (require (prefix-in interp- "top-interp.scm"))
  
  (provide interp-run run-all)

  (define run-all
    (lambda ()
      (interp-run-all)))

   ;;; interface for book test ;;;
 ; (provide test-all)
  ;(define (test-all) 
 ;   (run-all))
  (provide add-test)
  (define (add-test) 
    (run-all))


  ;;
  (define (fact n cont)
    (if (= n 0)
        (cont 1)
        (fact (- n 1) (lambda (x) (cont (* n x))))
    ))

  (trace fact)
  (display (fact 5 (lambda (x) x)))
  (newline)

  ;;; Run tests
  (run-all)
  
)


