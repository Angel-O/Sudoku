#lang racket

;making everyhting accessible for unit tests
(provide (all-defined-out))

(require racket/set)

; start
(define matrix (list
                (list 0 2 5 0 0 1 0 0 0)
                (list 1 0 4 2 5 0 0 0 0)
                (list 0 0 6 0 0 4 2 1 0)
                (list 0 5 0 0 0 0 3 2 0)
                (list 6 0 0 0 2 0 0 0 9)
                (list 0 8 7 0 0 0 0 6 0)
                (list 0 9 1 5 0 0 6 0 0)
                (list 0 0 0 0 7 8 1 0 3)
                (list 0 0 0 6 0 0 5 9 0)))

(define my-set (set 0 1 7 9))

(define your-set (set 0 1 0 7 9 7 0 0 7))

(define sets-are-equal (equal? my-set your-set))

(define row (list 1 2 3 4 5 6 7 8 9))

(define (transform2 ll)
  (let [(tt null)] ; empty list temporary storage
    (cond [(= 0 (car ll)) (cons 1 2 3 4 5 6 7 8 9 tt)]
          [#t (car ll)])))

; steps
; 1 go through each element in the list
;   if it's zero add the set of all nums to the temporary list
;   otherwise add a singleton set
; 2 repeat recursively (call the same function on the cdr of the original list)
; hint: use the map function 

;processes a single list turning 0s into whole sets
;and all other numbers into singleton sets
(define (process-inner-list l)
  (map (lambda (x)
         (cond[(= 0 x) (set 1 2 3 4 5 6 7 8 9)]
              [#t (set x)])) l ))

;processes a list of lists applying the
;transformation defined above to each element
;of the containing list
(define (transform ll)
  (map (lambda (l)
         (cond[(null? l) set(l)]
              [#t (process-inner-list l)])) ll))

;removes an element from a set excluding the
;singleton set that contains that number
(define (rm-from-set ss num)
  (cond[(= 1 (set-count ss)) ss]
       [(set-member? ss num)(set-remove ss num)]
       [#t ss]))

;establish whether or not a set contains a unique element
;NOTE: will return false for empty sets 
(define (is-singleton a-set)
  (= 1 (set-count a-set)))

;establish if a list has all singleton sets
;NOTE: will return true for empty lists
(define (has-all-singleton ll)
  (cond[(null? ll) #t] ; will handle the case of an empty list...if a list has one element this will be the next step in the recursion
       [(is-singleton (car ll)) (has-all-singleton (cdr ll))]
       [#t #f])) ; default ==> return false

; same row => same inner-list
; same column => same index...(let ?? let* ??)
; same 3x3 box => recursion ?? each box is a submatrix...

; processes a list of sets ===> ls
;(define (process-row ls) 
;  (map (lambda (s)
;         (cond[(is-singleton s) (set-first s)]
;              [#t "multi"])) ls ))

(define (process-set s el) 
  (cond[(is-singleton s) s]
       [#t (set-map s (lambda (x)
                        (cond [(= x el) (set-remove s el)]
                              [#t "not found"])))]))

; given a list of sets and a singleton-subset it will remove
; the value contained in the singleton from all sets in the list
(define (remove-subset-from-sets ls subset)
  (map (lambda (a-set)
         (cond[(set-empty? a-set) a-set]
              [(proper-subset? subset a-set) (set-remove a-set (set-first subset))]
              [#t a-set])) ls))

; processes a ls ==> list of sets
; if car singleton => remove from other set
; if not singleton ==> recursive call
; if no removal ===> stop
(define (process-row3 ls) 
  (cond [(null? ls) ls]
        [(is-singleton (car ls)) (remove-subset-from-sets ls (car ls))]
        [#t (process-row (cdr ls))]))

;(define (remove-subset ls subset)
;  (cond[(null? ls) ls]
;       [(proper-subset? subset (car ls)) ...]

(define (process-row ls) 
  (map (lambda (s)
          (cond[(is-singleton s) (remove-subset-from-sets ls s)]
               [#t (process-row (cdr ls))])) ls ))

; subtract two sets only if they are different
(define (safe-subtract s1 s2)
  (cond[(set=? s1 s2) s1]
       [#t (set-subtract s1 s2)]))

; for each sinlgeton passed as second argument it will
; remove the elemnt enclosed in its set from the set
; passed as first parameter
(define (rm-singl s singletons)
  (cond[(null? (cdr singletons)) (safe-subtract s (car singletons))]
       [#t (let [(new-set (safe-subtract s (car singletons)))] ; create a new set that does not contain the first singleton
             (rm-singl new-set (cdr singletons)))])) ; remove the remaining (2nd, 3rd,...) singletons from the set

; will process a row to reduce it to a list of singletons
(define (process-row2 ls)
   (let ([singletons (filter (lambda(s) ; extract all the singletons in the current list
                             (is-singleton s)) ls)])
     (map (lambda (s)
             (rm-singl s singletons)) ls))) ; remove the singletons from each one of them


         

; processes a ls ==> list of sets
; if car singleton => remove from other set
; if not singleton ==> recursive call
; if no removal ===> stop


(define (solve lls) ; the transformed matrix is a lls (list of list of sets) ===> each element is a list
  (cond[(has-all-singleton (car lls)) (solve (cdr lls))] ; if the list has been reduced to all singletons move on...
       [(let ((a-list (car lls))) (process-row2 a-list))])) ; extract the initial list of the matrix...
  
(define tr (transform matrix))

