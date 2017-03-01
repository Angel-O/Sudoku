#lang racket

;making everyhting accessible for unit tests
(provide (all-defined-out))

(define x 3)
(define cube
  (lambda (x)
    (* x (* x x))))

(define (sign x)
  (cond [(< x 0) "negative"]
        [(> x 0) "positive"]
        [#t "zero"])) ;set the last condition  as true: it's the equivalent of the default clause in a switch statement

;practicing with let: syntax ====> let((var1 binding1) (var2 binding2)....)

(define (double-mult x)
  (let [(y 2)
        (z 5)]
    (* x y z)))


(define (f x)
  (let [(y 3) (z 1/3)]
    (* x y z)))

; with let variable decalred in inner scope
(define (strictly-bigger-than-all x ll)
  (cond [(null? ll) #t]
        [(<= x (car ll)) #f]
        [#t
         (let [(nl (cdr ll))]
           (strictly-bigger-than-all x nl))]))

 ; without let variable 
 (define (strictly-bigger-than-all-no-let x ll)
  (cond [(null? ll) #t]
        [(<= x (car ll)) #f]
        [#t (strictly-bigger-than-all-no-let x (cdr ll))]))
