(module project1 mzscheme
  ;;;;;;;;;;;; Comp 301 Project 1 ;;;;;;;;;;;;;;;;
  ;;; Add group members below
  ;;; Alpay Sabuncuoğlu, asabuncuoglu13, 0011221
  ;;; Gül Sena Altıntaş, galtintas17, 0011222
  ;;; save your file in the format: p1_0011221_asabuncuoglu13_00112222_galtintas17.rkt
  ;;;;;;;;;;;;;;;;;;; PROBLEMS ;;;;;;;;;;;;;;;;;;;;
  ;; PROJECT 1 Part A | Write your answer below here as a comment
  ;
  ; Our chosen quantity to represent is a string of alphabetical lowercase characters
  ;
  ; Representation1:
  ; we represent an alphabetical string of lowercase characters with a reversed list of the shifted value of th ascii of each character
  ; The ascii shifted value assumed the character 'a' has zero ascii value.
  ;
  ; ascii-rep = ()
  ;	      = (Integer string-ascii) 
  ;
  ; Representation2:
  ; we represent an alphabetical string of lowercase characters with a reversed list of characters of the string
  ;
  ; char-rep = ()
  ;	     = (Character char-rep)
  ;
  ;
  ;; PROJECT 1 Part B
  ;; First Representation | We added a suffix so that both first and second representations can be tested at once.

  (define CHARS "abcdefghijklmnopqrstuvwxyz")
  (define CHARS-LEN 26)

  (define reverse-list
    (lambda (x)
      (if(null? x)
      '()
      (append (reverse-list (cdr x)) (list (car x))))))
  
  (define get-num
    (lambda (char index)
      (cond
        ((= index CHARS-LEN) 'non-alphabetic-character)
        ((equal? char (string-ref CHARS index)) index)
        (else (get-num char (+ index 1)))
        )))
  
  (define get-ascii
    (lambda (string index)
     (if (< index (string-length string)) 
     (cons (get-num (string-ref string index) 0) (get-ascii string (+ index 1)))
     '()
      )))
  
  (define create-a
    (lambda (string)
      (reverse-list (get-ascii string 0))
      ))

  (define is-zero-a?
    (lambda (ascii-string) (null? ascii-string)))

  (define next-char
    (lambda (chars) (cadr chars)))
  
  (define this-entry
    (lambda (x) (car x)))
  (define next-entry
    (lambda (x) (cdr x)))
  
  (define successor-helper
    (lambda (string-ascii index)
      (cond
        ((null? string-ascii) (list 0))
        ((= (this-entry string-ascii) 25) (cons 0 (successor-helper (next-entry string-ascii) (+ index 1))))
        (else (cons (+ (this-entry string-ascii) 1) (next-entry string-ascii)))
        )))
  (define successor-a
    (lambda (string-ascii)
      (successor-helper string-ascii 0)
      ))


  ;; Second Representation | We added a -b suffix so that both Unary and BigNum can be tested at once.

  (define (create-b string)
    (define (create-b-helper string ind)
      (if (>= ind (string-length string))
          '()
          (cons (string-ref string ind) (create-b-helper string (+ ind 1)))))
    (reverse-list (create-b-helper string 0)))

  (define (is-zero-b? char-string)
    (eqv? char-string '()))

  (define (char->ind char)
    (define (inner char ind)
      (if (equal? char (string-ref CHARS ind))
          ind
          (inner char (+ ind 1))))
    (inner char 0))
    
  (define (successor-b char-string)
    (cond
      ((is-zero-b? char-string) (list (string-ref CHARS 0)))
      ((equal? (char->ind (car char-string)) (- CHARS-LEN 1)) (cons (string-ref CHARS 0) (successor-b (cdr char-string))))
      (else
       (let ((successor-ind (modulo (+ (char->ind (car char-string)) 1) CHARS-LEN)))
         (cons (string-ref CHARS successor-ind) (cdr char-string))))))

  ;; PROJECT 1 Part C | Write your answer below here as a comment
  ;
  ; Constructors: Part of the data type interface which builds elements of the data-type
  ; Observers: Part of the data type interface which extracts information from values of the data-type
  ; Extractors: Observers that extract portions of the data type value
  ; Predicates: Observers that validate whether an expressions is a representation of certain data-type
  ;
  ;
  ; create-a: constructor
  ; is-zero-a?: observer-predicate
  ; successor-a: constructor
  ; create-b: constructor 
  ; is-zero-b?: observer-predicate
  ; successor-b: constructor
  ;
  ;
  ;
  ;
  ;
  ;
  
  ;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Don't worry about the below function, we included it to test your implemented functions and display the result in the console
  ;; As you implement your functions you can Run (the button on the top right corner) to test your implementations
  (define-syntax equal??
    (syntax-rules ()
      ((_ test-exp correct-ans)
       (let ((observed-ans test-exp))
         (if (not (equal? observed-ans correct-ans))
           (printf "Oops! ~s returned ~s, should have returned ~s~%" 'test-exp observed-ans correct-ans)
           (printf "Correct! ~s => ~s~%" 'test-exp correct-ans))))))
  
  
  ;; PROJECT 1 Part D | Remove the comments and write test cases.
  (display "First Representation Tests\n")
  (equal?? (create-a "abcd") '(3 2 1 0)) ; should return ?
  (equal?? (is-zero-a? '(0)) #f) ; should return #f
  (equal?? (is-zero-a? '()) #t) ; should return #t
  (equal?? (successor-a '(3 2 1 0)) '(4 2 1 0)) ; should return ?
  (newline)

  
  (display "Second Representation Tests\n")
  (equal?? (create-b "abcd") '(#\d #\c #\b #\a)) ; should return  (#\d #\c #\b #\a)
  (equal?? (is-zero-b?  '(#\d #\a)) #f) ; should return #f
  (equal?? (is-zero-b? '()) #t) ; should return #t
  (equal?? (successor-b '(#\d #\c #\b #\a)) '(#\e #\c #\b #\a)) ; should return '(#\e #\c #\b #\a)
  (equal?? (successor-b '(#\z #\z #\z #\z)) '(#\a #\a #\a #\a #\a)) ; should return '(#\e #\c #\b #\a)
  (equal?? (successor-b (create-b "kfzz")) '(#\a #\a #\g #\k)) ; should return '(#\e #\c #\b #\a)
  (newline)
)
