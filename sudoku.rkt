#lang racket

; TO_DO run discover-singleton
;also in individual squares!!!! test (att3(att3(att3 tr))) => check square @ (6 3) #1 is a potential singleton, but it is not removed...

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


(define row (list 1 2 3 4 5 6 7 8 9))

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

(define (has-no-singleton ll)
  (cond[(null? ll) #t] ; will handle the case of an empty list...if a list has one element this will be the next step in the recursion
       [(not (is-singleton (car ll))) (has-no-singleton (cdr ll))]
       [#t #f])) ; default ==> return false

; given a list of sets and a singleton-subset it will remove
; the value contained in the singleton from all sets in the list
(define (remove-subset-from-sets ls subset)
  (map (lambda (a-set)
         (cond[(set-empty? a-set) a-set]
              [(proper-subset? subset a-set) (set-remove a-set (set-first subset))]
              [#t a-set])) ls))

; subtract two sets only if they are different
(define (safe-subtract s1 s2)
  (cond[(set=? s1 s2) s1]
       [#t (set-subtract s1 s2)]))

; for each sinlgeton passed as second argument it will
; remove the element enclosed in its set from the set
; passed as first parameter
(define (rm-singl s singletons)
  (cond[(null? (cdr singletons)) (safe-subtract s (car singletons))]
       [#t (let [(new-set (safe-subtract s (car singletons)))] ; create a new set that does not contain the first singleton
             (rm-singl new-set (cdr singletons)))])) ; remove the remaining (2nd, 3rd,...) singletons from the set


; extract all the singletons in the current list
(define (find-singletons ls)
  (filter (lambda (s) 
            (is-singleton s)) ls))

; will process a row to reduce it to a list of singletons
(define (remove-singletons ls)
   (let ([singletons (find-singletons ls)])
     (map (lambda (s)
             (rm-singl s singletons)) ls))) ; remove the singletons from each one of the sets in the current list

;(define (find-unique ls)
  

(define (solve-rows lls) ; the transformed matrix is a lls (list of list of sets) ===> each element is a list
  (cond[(null? lls) lls] ;;!!!!
       [(has-all-singleton (car lls)) (solve-rows (cdr lls))] ; if the list has been reduced to all singletons move on...
       [#t (map (lambda (ls)
               (remove-singletons ls)) lls)])) ; extract the initial list of the matrix...

(define tr (transform matrix))

(define (rotate lls)
  (cond[(null? lls) (error "empty matrix")]
       [#t (apply map list lls) ]))


; new strategy
; remove singletons from rows
; rotate matrix and do the same
     
;given a list of sets and an element it returns
;true if that element is not contained in any of the sets
; in the list
(define (unique? el ls)
  (if (null? ls)
      el ; return the element if we got to the end of the list with no failing tests!!
      (let([a-set (car ls)]
           [a-singleton (set el)]) ; make a singleton out of an element
        (cond[(subset? a-singleton a-set) #f]
             [#t (unique? el (cdr ls))]))))


(define (replace-set-with-singleton a-set ls)
  (let* ([slist (set->list a-set)]
         [singleton-list (filter (lambda (el) ; get a list of all elements ttha could be singleton in the current row
                             (unique? el (remove a-set ls))) slist)])
     (if (null? singleton-list)
        ls
        (let([singleton (set (car singleton-list))]) ; make a singleton out of the first element of the list just found (only the first element is needed)
          (replace a-set ls singleton))))) 

; replaces set1 in ls with set2
(define (replace set1 ls set2)
  (map (lambda (s)
         (if (set=? s set1)
             set2
             s)) ls))

; get a copy or a count to check if stuff has changed!!!

(define (att3 lls)
  (let* ([before (count-all-singletons lls)])
    (if (resolved? lls)
        lls
        (cond;[(= before (count-all-singletons (do-stuff lls))) (do-stuff lls)] ; stop if no progress are made 
             [#t (att3 (do-stuff lls))]))))

(define corners (list '(0 0) '(0 3) '(0 6) '(3 0) '(3 3) '(3 6) '(6 0) '(6 3) '(6 6) )) 

(define (do-stuff lls)
  (cond[(resolved? lls) lls]
       [#t
        (do-stuff

  (solve-rows
   (rotate
    (solve-square
     (discover-singleton
      (solve-rows
       (rotate
        (solve-square
         (discover-singleton
          (solve-rows lls)) corners)))) corners))))]))


;(define (do-stuff2 lls)
  
;  (rotate(solve-square (discover-singleton (solve-rows lls)) corners)))
  


(define (discover-singleton2 lls)
  (map (lambda (ls)
         create-singleton ls) lls))

(struct singletonfound (value set) #:transparent)

(define (discover-singleton lls)
  (map (lambda (ls)
         (let* ([singletons (get-them (create-singleton ls))]) ;; get the potential singletons
           (eliminate-sing ls singletons))) lls))

(define (eliminate-sing ls singletons)
  (cond[(null? singletons) ls] ; if there aren't any return the same list
       [#t (let* ([xx (car singletons)]) ; otherwise replace and call recursively
                      (eliminate-sing (replace (singletonfound-set xx) ls (set (singletonfound-value xx))) (cdr singletons)))]))

;!!!!! takes a list of sets and checks if any
; set has an element that does not appear any
; other set: if so it makes a singleton out of it
;(define (create-singleton ls)
;  (let* ([a-set (car ls)]
;         [a-list (remove a-set ls)])
;    (cond[(number? (unique? 0 ls)) ls] ; do stuff !!!!!!!...A_LIST
;         [#t create-singleton (cdr ls)])))



(define (get-them lx)
  (let ([flx (flat '() lx)])
    (filter (lambda(x)
            (singletonfound? x)) flx)))

(define (create-singleton ls)
  (map (lambda(s)
            (let* ([a-list (remove s ls)]
                   [slist (set->list s)])
              (map (lambda (num)
                     (cond[(number? (unique? num a-list)) (singletonfound num s)] ; do stuff !!!!!!!
                          [#t create-singleton (cdr ls)])) slist))) ls))

; determines if the matrix has been resolved
(define (resolved? lls)
  (cond [(= (length lls) (length (filter has-all-singleton lls))) #t]
        [#t #f]))

; counts the total number of singletons in the matrix
(define (count-all-singletons lls)
  (foldl (lambda (ls total)
           (+ total (length ls))) 0 (map get-singletons lls)))

; returns a list containing all singleton sets in the list
(define (get-singletons ls)
  (filter (lambda (s)
            (is-singleton s)) ls))

; for each row in a matrix gets the first three elements 
(define (get-square lls row col)
  (let([rows (restrict-range lls row)])
    (map (lambda (ls)
              (restrict-range ls col)) rows)))

; restricts the collection range based on the index provided  
; index-of not working here!!! same sets have the same index!!
(define (restrict-range coll index)
  (cond[(and (>= index 0)(< index 3)) (list (first coll) (second coll) (third coll))]
       [(and (>= index 3)(< index 6)) (list (fourth coll) (fifth coll) (sixth coll))]
       [(and (>= index 6)(< index 9)) (list (seventh coll) (eighth coll) (ninth coll))]
       [#t (error "bla bla")]))

(define (restrict-range2 coll index)
  (cond[(null? coll) coll]
       [(and (>= index 0)(< index 3)) (list (first coll) (second coll) (third coll))]
       [(and (>= index 3)(< index 6)) (list (fourth coll) (fifth coll) (sixth coll))]
       [(and (>= index 6)(< index 9)) (list (seventh coll) (eighth coll) (ninth coll))]
       [#t (error "bla bla")]))

; gets the square based on the coordinates and
; reduces the sets in the (3 x 3) matrix
(define (process-square lls row col)
  (let* ([square (get-square lls row col)]
        [singletons (get-singletons-in-square square)])
    (map (lambda (ls)
           (remove-s ls singletons)) square)))

; removes a list of singletons from a list of sets
(define (remove-s ls singletons)
  (cond[(null? singletons) ls]
       [#t (remove-s (remove-subset-from-sets ls (car singletons)) (cdr singletons))]))

;(define (solve-square2 lls row col)
;  (let ([new-square
;         (let([square (get-square lls row col)])
;                      (process-square lls row col))])
;    (do-replacement new-square lls row col)))

;(define (solve-square lls corners)
;  (cond[(null? (cdr corners)) lls]
;       [#t (solve-square
;            (let* ([row (caar corners)]
;                   [col (last(car corners))]
;                   [new-square (process-square lls row col)])
;              (do-replacement new-square lls row col)) (cdr corners))]))

(define (solve-square lls corners)
  (cond[(null? corners) lls]
       [#t (let* ([row (caar corners)]
                  [col (last (car corners))]              
                  [new-square (process-square lls row col)]
                  [new-matrix (do-replacement new-square lls row col)])
               (solve-square new-matrix (cdr corners)))]))
  

; change of plan: row number can only be 0, 3, 6 and nothing else!!
(define (do-replacement new-square lls row col)
  (map (lambda (ls)
         (cond[(= (index-of lls ls) row) (build-row ls (first new-square) col)]
              [(= (index-of lls ls) (+ row 1)) (build-row ls (second new-square) col)]
              [(= (index-of lls ls) (+ row 2)) (build-row ls (third new-square) col) ]
              [#t ls])) lls)) ; return the row as it was

; change of plan: col number can only be 0, 3, 6 and nothing else!!
(define (build-row ls square-row col)
  (cond[(= col 0)(flat '() (list square-row (cdddr ls)))] ; discarding the firs three
       [(= col 3)(flat '() (list (take ls 3) square-row (take-right ls 3)))] ;insert in between
       [(= col 6)(flat '() (list (take ls 6) square-row))])) ;insert at the end
  

; get a list of all singletons in a matrix
(define (get-singletons-in-square lls)
  (let ([singletons (map (lambda (ls)
                           (get-singletons ls)) lls)])
        (flat '() singletons)))

; flat-map
(define (flat start nested)
  (cond[(null? nested) start]
       [(null? (cdr nested)) (append start (car nested))]
       [#t (flat (append start (car nested)) (cdr nested))]))
  

  