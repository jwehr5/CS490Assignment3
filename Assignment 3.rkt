#lang racket
(require data/either)
(require data/maybe)

;(display "Enter a number: ")
;(list (read-line (current-input-port) 'any))

;Function for pushing things onto the stack
(define (push stack item-to-push)
  (append item-to-push stack))

;Function for popping the stack
(define (pop stack)
  (rest stack))

;This breaks the user input from a single string into series of strings i.e "3 5 ADD" becomes "3" "5" "ADD".
(define (input-to-list lst)
  (if (empty? lst)
      lst
      (append (string-split (first lst)) (input-to-list (rest lst)))))

;This takes the numbers that are strings and converts them into regular numbers
(define (convert-string-to-numbers lst new-list)

  (if (empty? lst)
      new-list
      (if (number? (string->number (first lst)))
          (convert-string-to-numbers (rest lst) (append new-list (list (string->number (first lst)))))
          (convert-string-to-numbers (rest lst) (append new-list (list (first lst)))))))

;Function for doing all our operations
(define (perform-operation stack op)
  (if (>= (length stack) 2)
      (safe-operation stack op)
      (error "Error: Too Few Arguments")))

;If the stack has enough arguments, then this function will run
(define (safe-operation stack op)
  (define operand1 (second stack))
  (define operand2 (first stack))

  ;(display stack)
  ;(remove operand1 (remove operand2 (push stack (list (+ (string->number operand1) (string->number operand2))))))
  (if (and (equal? operand2 0) (equal? op /))
      (error "Error division by 0")
      (push (pop (pop stack)) (list (op  operand1 operand2)))))

  

(define (calculate input-list input-stack)

  ;(display input-stack)
  (if (empty? input-list)
       input-stack
      (cond
      
        [(number? (first input-list)) (calculate (rest input-list) (push input-stack (list (first input-list))))]
        [(string-ci=? "ADD" (first input-list)) (calculate (rest input-list)  (perform-operation input-stack +))]
        [(string-ci=? "SUB" (first input-list)) (calculate (rest input-list)  (perform-operation input-stack -))]
        [(string-ci=? "MUL" (first input-list)) (calculate (rest input-list)  (perform-operation input-stack *))]
        [(string-ci=? "DIV" (first input-list)) (calculate (rest input-list)  (perform-operation input-stack /))]


        
        [(and (string-ci=? "CLR" (first input-list)) (>= (length input-stack) 1)) (calculate (rest input-list) '())]
        [(and (string-ci=? "SHOW" (first input-list)) (>= (length input-stack) 1)) (print input-stack) (calculate (rest input-list) input-stack)]
        [(and (string-ci=? "TOP" (first input-list)) (>= (length input-stack) 1))  (print (first input-stack)) (calculate (rest input-list) input-stack)]
        [(and (string-ci=? "SIZ" (first input-list)) (>= (length input-stack) 1))  (print (length input-stack)) (calculate (rest input-list) input-stack)]
        [(and (string-ci=? "DUP" (first input-list)) (>= (length input-stack) 1)) (calculate (rest input-list) (push input-stack (list (first input-stack))))]
        [(string-ci=? "END" (first input-list)) "Done"]
        [else (failure "Error: Unknown command")])))



(define (get-input stack)
  (cond
    [(equal? stack "Done") "Done"]
    [else (display "Enter some numbers or commands: ")
          (define input-string (list (read-line (current-input-port) 'any)))
          (define input-list (convert-string-to-numbers (input-to-list input-string) '()))
          (get-input (calculate input-list stack))]))

   #|
  ;Read the input as a list
  (display "Enter some numbers or commands: ")
  (define input-string (list (read-line (current-input-port) 'any)))
  
  ;Break the input up making each value in the input its own value in the list
  (define input-list (convert-string-to-numbers (input-to-list input-string) '()))
  |#
  
  
  
  ;Process the items in the list
  #|
  (if (equal? stack "Done")
      "Done"
      (get-input (calculate input-list stack))))
  |#

  



;Main Program
(get-input '())


