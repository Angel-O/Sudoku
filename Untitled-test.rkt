#lang racket

(require rackunit "Untitled.rkt")

;; ======================== SUDOKU TESTS ===================

;; Testing the "transform" and "solve" functions of the sudoku solver
;; Note: the test might produce output which is unrelated to the test
;; itself (it is simply the side effects of running the solve
;; function)

;; =============== defining helpers sets ===================

(define multi-set (set 1 2 3 4 5 6 7 8 9))
(define set-one (set 1))
(define set-two (set 2))
(define set-three (set 3))
(define set-four (set 4))
(define set-five (set 5))
(define set-six (set 6))
(define set-seven (set 7))
(define set-eight (set 8))
(define set-nine (set 9))

;; =============== defining helpers predicates =============

; checking that a list contains all numbers different from zero
(define (all_numbers ln)
  (cond[(empty? ln) #t]
         [#t (and (number? (car ln)) (not(eq? (car ln) 0)) (all_numbers (cdr ln)))]))

;; ================ defining custom tests ==================

; test to verify that a matrix contains only numbers
; (rather than sets, as it would be if the matrix cannot be solved)
; note: this is run at the bottom of this file
(define-simple-check (check-singletons? lls)
    (cond[(empty? lls) #t]
         [#t (and (all_numbers (car lls)) (check-singletons? (cdr lls)))]))

;; ================ defining helpers matrices ==============

; solution rotated
(define rotated (rotate (solve matrix)))

; solution
(define solved (solve matrix))

; alias for an unsolved matrix
(define unsolved matrix)

; matrix with not enough non-zero elements
(define too-few (list 
                 (list 0 2 5 0 0 1 0 0 0)
                 (list 1 0 4 2 0 0 0 0 0)
                 (list 0 0 6 0 0 4 2 1 0)
                 (list 0 5 0 0 0 0 3 2 0)
                 (list 6 0 0 0 2 0 0 0 9)
                 (list 0 8 0 0 0 0 0 6 0)
                 (list 0 0 1 5 0 0 6 0 0)
                 (list 0 0 0 0 7 8 1 0 3)
                 (list 0 0 0 6 0 0 5 0 0)))

; matrix with duplicate entries
(define has-dup (list
                 (list 2 2 5 0 0 1 0 0 0)
                 (list 1 0 4 2 5 0 0 0 0)
                 (list 0 0 6 0 0 4 2 1 0)
                 (list 0 5 0 0 0 0 3 2 0)
                 (list 6 0 0 0 2 0 0 0 9)
                 (list 0 8 7 0 0 0 0 6 0)
                 (list 0 9 1 5 0 0 6 0 0)
                 (list 0 0 0 0 7 8 1 0 3)
                 (list 0 0 0 6 0 0 5 9 0)))

;; ======================= transform TESTS ===============

;; transform should turn non-zero elements into singleton sets
(check-equal? (transform (list (list 1))) (list (list set-one)) "A 'one' should turn into a singleton set")
(check-equal? (transform (list (list 2))) (list (list set-two)) "A 'two' should turn into a singleton set")
(check-equal? (transform (list (list 3))) (list (list set-three)) "A 'three' should turn into a singleton set")
(check-equal? (transform (list (list 4))) (list (list set-four)) "A 'four' should turn into a singleton set")
(check-equal? (transform (list (list 5))) (list (list set-five)) "A 'five' should turn into a singleton set")
(check-equal? (transform (list (list 6))) (list (list set-six)) "A 'six' should turn into a singleton set")
(check-equal? (transform (list (list 7))) (list (list set-seven)) "A 'seven' should turn into a singleton set")
(check-equal? (transform (list (list 8))) (list (list set-eight)) "A 'eight' should turn into a singleton set")
(check-equal? (transform (list (list 9))) (list (list set-nine)) "A 'nine' should turn into a singleton set")

;; transform should turn a zero element into a multi set
(check-equal? (transform (list (list 0))) (list (list multi-set)) "A 'zero' should turn into a multi set")

;; ======================== solve TESTS ===================

;; trying to solve a matrix with duplicates should throw an error
(check-exn exn:fail? (lambda () (solve has-dup)) "Solving a matrix with duplicates should raise an error")

;; each row in the solution has different values
(check-match (car solved) (list q w e r t y u i o))
(check-match (cadr solved) (list q w e r t y u i o))
(check-match (caddr solved) (list q w e r t y u i o))
(check-match (cadddr solved) (list q w e r t y u i o))
(check-match (fourth solved) (list q w e r t y u i o))
(check-match (fifth solved) (list q w e r t y u i o))
(check-match (sixth solved) (list q w e r t y u i o))
(check-match (seventh solved) (list q w e r t y u i o))
(check-match (eighth solved) (list q w e r t y u i o))
(check-match (ninth solved) (list q w e r t y u i o))

;; each column in the solution has different values
(check-match (car rotated) (list q w e r t y u i o))
(check-match (cadr rotated) (list q w e r t y u i o))
(check-match (caddr rotated) (list q w e r t y u i o))
(check-match (cadddr rotated) (list q w e r t y u i o))
(check-match (fourth rotated) (list q w e r t y u i o))
(check-match (fifth rotated) (list q w e r t y u i o))
(check-match (sixth rotated) (list q w e r t y u i o))
(check-match (seventh rotated) (list q w e r t y u i o))
(check-match (eighth rotated) (list q w e r t y u i o))
(check-match (ninth rotated) (list q w e r t y u i o))

;; each 3 x 3 square in the solution has different values
(check-match solved (list
                     (list a b c _ _ _ _ _ _) ; top left square
                     (list d e f _ _ _ _ _ _)
                     (list g h i _ _ _ _ _ _)
                      _ _ _ _ _ _))
(check-match solved (list
                     (list _ _ _ a b c _ _ _) ; top center square
                     (list _ _ _ d e f _ _ _)
                     (list _ _ _ g h i _ _ _)
                      _ _ _ _ _ _))
(check-match solved (list
                     (list _ _ _ _ _ _ a b c) ; top right square
                     (list _ _ _ _ _ _ d e f)
                     (list _ _ _ _ _ _ g h i)
                     _ _ _ _ _ _))
(check-match solved (list
                     _ _ _
                     (list a b c _ _ _ _ _ _) ; mid left square
                     (list d e f _ _ _ _ _ _)
                     (list g h i _ _ _ _ _ _)
                      _ _ _))
(check-match solved (list
                     _ _ _
                     (list _ _ _ a b c _ _ _) ; mid center square
                     (list _ _ _ d e f _ _ _)
                     (list _ _ _ g h i _ _ _)
                      _ _ _))
(check-match solved (list
                     _ _ _
                     (list _ _ _ _ _ _ a b c) ; mid right square
                     (list _ _ _ _ _ _ d e f)
                     (list _ _ _ _ _ _ g h i)
                     _ _ _))
(check-match solved (list
                     _ _ _ _ _ _
                     (list a b c _ _ _ _ _ _) ; bottom left square
                     (list d e f _ _ _ _ _ _)
                     (list g h i _ _ _ _ _ _)))
(check-match solved (list
                     _ _ _ _ _ _
                     (list _ _ _ a b c _ _ _) ; bottom center square
                     (list _ _ _ d e f _ _ _)
                     (list _ _ _ g h i _ _ _)))
(check-match solved (list
                     _ _ _ _ _ _
                     (list _ _ _ _ _ _ a b c) ; bottom right square
                     (list _ _ _ _ _ _ d e f)
                     (list _ _ _ _ _ _ g h i))) 


;; all values in the solution are numbers different from zero (rather than sets, and therefore the sudoku has been solved)
(check-singletons? solved "A solved matrix should contain only numbers different from zero")

;; a sudoku with enough elements at start should be solvable
(check-equal? (solve unsolved) solved "A well defined sudoku should produce a solution")

;; a sudoku with too few elements at start should not be solvable
(check-not-equal? (solve too-few) solved "A poorly defined sudoku should not produce a solution")



