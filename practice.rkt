#lang racket

(require racket/set)

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

(define (do-something x)
  (let[(z 9)
       (y 2)]
    (* (+ z y) x)))

(define (dangerous x y)
  (cond [(> y 0) (/ x y)]
        [(< y 0) (/ x y)]
        [#t "division by zero"]))

; function in function (currying ???)
; increment after multiply by ten

(define (iambt u)
  (lambda (u) (* 10 u)))



; a struct is a like a record
; it gets  a ? function

(struct person (name) #:transparent)

(define my-set (set 0 1 7 9))

(define your-set (set 0 1 0 7 9 7 0 0 7))

(define sets-are-equal (equal? my-set your-set))


;; steps
;; if singleton remove from other
;; if not see if it contains an element not present in the same row...
;;
;;
;;
;;