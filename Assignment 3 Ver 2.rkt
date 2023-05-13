#lang racket
(require data/either)
(require data/maybe)


;Function for pushing things onto the stack.
(define (push stack item-to-push)
  (append item-to-push stack))

;Function for popping the stack.
(define (pop stack)
  (rest stack))

;This breaks the user input from a single string into series of strings. Ex: "3 5 ADD" becomes "3" "5" "ADD".
(define (input-to-list lst)
  (if (empty? lst)
      lst
      (append (string-split (first lst)) (input-to-list (rest lst)))))

;This takes the numbers that are strings and converts them into regular numbers.
(define (convert-string-to-numbers lst new-list)

  (if (empty? lst)
      new-list
      (if (number? (string->number (first lst)))
          (convert-string-to-numbers (rest lst) (append new-list (list (string->number (first lst)))))
          (convert-string-to-numbers (rest lst) (append new-list (list (first lst)))))))

;Function for doing all our operations
(define (perform-operation stack op)
  #|
  First off check to make sure the length of the stack of is 2 or greater before we carry out the operation.
  If the stack size is less than 2 then we return a failure indicating that there are not enough arguments.
  If we have enough arguments, call the safe-operation function.
  If we return a success from the safe-operation function, then also return a success from this function indicating that everything went well.
  |#

  (cond
    [(not (>= (length stack) 2)) (display stack) (failure "Error: Too Few Arguments")]
    [else
     (cond
       [(success? (safe-operation stack op)) (success (from-success #f (safe-operation stack op)))]
       [else (display stack) (failure (from-failure #f (safe-operation stack op)))])]))

;If the stack has enough arguments, then this function will run.
(define (safe-operation stack op)
  ;This is the left operand.
  (define operand1 (second stack))

  ;This is the right operand.
  (define operand2 (first stack))


  ;Check to make sure we don't do division by 0;
  (if (and (equal? operand2 0) (equal? op /))
      (failure "Error: division by 0")
      (success (push (pop (pop stack)) (list (op  operand1 operand2))))))

  

(define (calculate input-list input-stack)

  ;If we have performed any operations check for a success or failure before proceeding.
  (cond
    [(success? input-stack) (calculate input-list (from-success #f input-stack))]
    [(failure? input-stack) (display "\n") input-stack]
    [else
         ;(display input-stack)
         (if (empty? input-list)

             ;If all the items were able to be processed, then return a success.
             (success input-stack)

             ;Determine which operation to do from the input.
             (cond
      
               [(number? (first input-list)) (calculate (rest input-list) (push input-stack (list (first input-list))))]
               [(string-ci=? "ADD" (first input-list)) (calculate (rest input-list)  (perform-operation input-stack +))]
               [(string-ci=? "SUB" (first input-list)) (calculate (rest input-list)  (perform-operation input-stack -))]
               [(string-ci=? "MUL" (first input-list)) (calculate (rest input-list)  (perform-operation input-stack *))]
               [(string-ci=? "DIV" (first input-list)) (calculate (rest input-list)  (perform-operation input-stack /))]

               ;The TOP and DUP commands cannot be performed on an empty stack.
               [(string-ci=? "CLR" (first input-list)) (calculate (rest input-list) '())]
               [(string-ci=? "SHOW" (first input-list)) (display input-stack) (display "\n") (calculate (rest input-list) input-stack)]
               [(and (string-ci=? "TOP" (first input-list)) (>= (length input-stack) 1))  (display (first input-stack)) (display "\n") (calculate (rest input-list) input-stack)]
               [(string-ci=? "SIZ" (first input-list))  (display (length input-stack)) (display "\n") (calculate (rest input-list) input-stack)]
               [(and (string-ci=? "DUP" (first input-list)) (>= (length input-stack) 1)) (calculate (rest input-list) (push input-stack (list (first input-stack))))]
               [(string-ci=? "END" (first input-list)) "Done"]
               [else (display input-stack) (display "\n") (failure "Error: Unknown command or tried performing command on an empty stack")]))]))



(define (get-input stack)
  
  (cond
    [(equal? stack "Done") "Done"]
    [(success? stack) (get-input (from-success #f stack))]
    [(failure? stack) (display (from-failure #f stack))]
    [else (display "Enter some numbers or commands: ")
          (define input-string (list (read-line (current-input-port) 'any)))
          (define input-list (convert-string-to-numbers (input-to-list input-string) '()))
          (get-input (calculate input-list stack))]))


;Main Program
(get-input '())


