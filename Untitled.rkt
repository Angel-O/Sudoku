#lang racket

;; making everyhting accessible for unit tests
(provide (all-defined-out))

(require racket/set)

;; the sudoku matrix to be solved
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

;; each pair represents the coordinates of a 3 x 3 submatrix 
(define corners (list '(0 0) '(0 3) '(0 6) '(3 0) '(3 3) '(3 6) '(6 0) '(6 3) '(6 6) ))

;; custom data structure containing the value of a potential singleton
;; set and the set that contains the potential value
(struct singletonfound (value set) #:transparent)

;; processes a single list of numbers (ln) turning 0s into a set
;; containing all numbers in the range 1-9 as to indicate that the
;; cell could hold any of them. Any other number is turned into a
;; singleton set, indicating that the cell is resolved
(define (transform-inner-list ln)
  (map (lambda (n)
         (cond[(= 0 n) (set 1 2 3 4 5 6 7 8 9)]
              [#t (set n)])) ln ))

;; processes a list of lists of sets (lls) applying the transformation defined
;; in "transform-inner-list" to each element of the containing list
(define (transform lls)
  (map (lambda (ls)
         (cond[(null? ls) set(ls)] ;safe check
              [#t (transform-inner-list ls)])) lls))

;; given a set it removes an element from it if the set contains the number.
;; NOTE: if the set is a singleton the number will not be removed
;(define (rm-from-set ss num)
;  (cond[(singleton? ss) ss]
;       [(set-member? ss num)(set-remove ss num)]
;       [#t ss]))

;; establish whether or not a set contains is a
;; singleton: that is it contains only one element
;; NOTE: will return false for empty sets 
(define (singleton? a-set)
  (= 1 (set-count a-set)))

;; establish if a list of sets has all singleton sets
;; NOTE: will return true for empty lists
(define (has-all-singleton ls)
  (cond[(null? ls) #t] ; will handle the case of an empty list...if a list has one element this will be the next step in the recursion
       [(singleton? (car ls)) (has-all-singleton (cdr ls))]
       [#t #f])) ; default ==> return false

;; given a list of sets and a singleton-subset it will remove
;; the value contained in the singleton from all sets in the list
(define (remove-subset-from-sets ls subset)
  (map (lambda (a-set)
         (cond[(set-empty? a-set) a-set]
              [(proper-subset? subset a-set) (set-remove a-set (set-first subset))]
              [#t a-set])) ls))

;; subtract two sets only if they are different
(define (safe-subtract s1 s2)
  (cond[(set=? s1 s2) s1]
       [#t (set-subtract s1 s2)]))

;; for each singleton passed as second argument it will
;; remove the element enclosed by the singleton from the set passed as first parameter
(define (rm-elements-from-set s singletons)
  (cond[(null? (cdr singletons)) (safe-subtract s (car singletons))]
       [#t (let [(new-set (safe-subtract s (car singletons)))] ; create a new set that does not contain the first singleton
             (rm-elements-from-set new-set (cdr singletons)))])) ; remove the remaining (2nd, 3rd,...) singletons from the set


;; extract all the singletons in the current list
(define (find-singletons ls)
  (filter (lambda (s) 
            (singleton? s)) ls))

;; will attempt to reduce each set in the given list by
;; eliminating the elemnts enclosed in the singletons
;; found in the list from the other sets in the list
(define (remove-singletons ls)
   (let ([singletons (find-singletons ls)])
     (map (lambda (s)
             (rm-elements-from-set s singletons)) ls))) ; remove the singletons from each one of the sets in the current list

;; scans the whole matrix in the attempt of reducing
;; each set in each row to a list of singletons
(define (reduce-rows lls) 
  (cond[(resolved? lls) lls] ; if the matrix is solved return it
       [#t (map (lambda (ls) ; otherwise reduce each row
               (remove-singletons ls)) lls)])) 

;; transpose the matrix so that horizontal lists
;; become vertical and vice-versa
(define (rotate lls)
  (cond;[(null? lls) (error "empty matrix")]
       [#t (apply map list lls) ]))

     
;; given a list of sets and an element it returns true
;; if that element is not contained in any of the sets
;; in the list: otherwise it will return the element searched for
(define (unique? el ls)
  (if (null? ls)
      el ; return the element if we got to the end of the list with no failing tests!!
      (let([a-set (car ls)]
           [a-singleton (set el)]) ; make a singleton out of an element
        (cond[(subset? a-singleton a-set) #f]
             [#t (unique? el (cdr ls))]))))

;; replaces set1 in ls with set2
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
             [#t (att3 (solve lls))]))))


; solves the sudoku: the order is important!!
(define (solve lls)
  (cond[(resolved? lls) lls]
       [#t
        (solve
         (reduce-rows
          (rotate
           (solve-square
            (discover-singleton
             (reduce-rows
              (rotate
               (solve-square
                (discover-singleton
                 (reduce-rows lls)) corners)))) corners))))]))


(define (discover-singleton lls)
  (map (lambda (ls)
         (let* ([singletons (get-them (create-singleton ls))]) ;; get the potential singletons
           (eliminate-sing ls singletons))) lls))

(define (eliminate-sing ls singletons)
  (cond[(null? singletons) ls] ; if there aren't any return the same list
       [#t (let* ([xx (car singletons)]) ; otherwise replace and call recursively
                      (eliminate-sing (replace (singletonfound-set xx) ls (set (singletonfound-value xx))) (cdr singletons)))]))




(define (get-them lx)
  (let ([flx (flat '() lx)])
    (filter (lambda(x)
            (singletonfound? x)) flx)))

;!!!!! takes a list of sets and checks if any
; set has an element that does not appear any
; other set: if so it makes a singleton out of it
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
            (singleton? s)) ls))

; for each row in a matrix gets the first three elements 
(define (get-square lls row col)
  (let([rows (restrict-range lls row)])
    (map (lambda (ls)
              (restrict-range ls col)) rows)))

; restricts the collection range based on the index provided  
; index-of not working here!!! same sets have the same index!!
(define (restrict-range coll index)
  (cond[(= index 0)(list (first coll) (second coll) (third coll))]
       [(= index 3)(list (fourth coll) (fifth coll) (sixth coll))]
       [(= index 6) (list (seventh coll) (eighth coll) (ninth coll))]
       [#t (error "invalid index provided")]))

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
  (cond[(= col 0)(flat '() (list square-row (cdddr ls)))] ; discarding the first three
       [(= col 3)(flat '() (list (take ls 3) square-row (take-right ls 3)))] ;insert in between
       [(= col 6)(flat '() (list (take ls 6) square-row))] ;insert at the end
       [#t (error "invalid index provided")])) 
  

; get a list of all singletons in a matrix
(define (get-singletons-in-square lls)
  (let ([singletons (map (lambda (ls)
                           (get-singletons ls)) lls)])
        (flat '() singletons)))

; flat-map
(define (flat start nested)
  (cond[(null? nested) start] ;safe check
       [(null? (cdr nested)) (append start (car nested))]
       [#t (flat (append start (car nested)) (cdr nested))]))
  

  