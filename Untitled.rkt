#lang racket

;; making everything accessible for unit tests
(provide (all-defined-out))

(require racket/set)

;; SUDOKU SOLVER
;; lls ==> list of lists of sets
;; ls  ==> list of sets
;; lx  ==> list of anything
;; s   ==> set

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

;; processes a list of lists of sets (lls) applying the transformation described
;; in the "transform-inner-list" function to each element of the containing list
(define (transform lls)
  (map (lambda (ls)
         (cond[(null? ls) set(ls)] ;safe check
              [#t (transform-inner-list ls)])) lls))

;; establish whether or not a set contains is a
;; singleton: that is it contains only one element
;; NOTE: will return false for empty sets 
(define (singleton? a-set)
  (= 1 (set-count a-set)))

;; establish if a list of sets has all singleton sets
;; NOTE: will return true for empty lists
(define (has-all-singleton ls)
  (cond[(null? ls) #t] ;will handle the case of an empty list...if a list has one element this will be the next step in the recursion
       [(singleton? (car ls)) (has-all-singleton (cdr ls))]
       [#t #f])) ;default ==> return false

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
(define (remove-elements-from-set s singletons)
  (cond[(null? (cdr singletons)) (safe-subtract s (car singletons))]
       [#t (let [(new-set (safe-subtract s (car singletons)))] ;create a new set that does not contain the first singleton
             (remove-elements-from-set new-set (cdr singletons)))])) ;remove the remaining (2nd, 3rd,...) singletons from the set

;; extract all the singletons in the current list
(define (find-singletons ls)
  (filter (lambda (s) 
            (singleton? s)) ls))

;; NOTE: native-singletons (as opposed to foreign singletons)
;; stands for singleton found in the list itself)
;; This function will attempt to reduce each set in the given list by
;; eliminating the elements (enclosed in the singletons
;; found in the list) from the other sets in the list
(define (remove-native-singletons ls)
   (let ([singletons (find-singletons ls)])
     (map (lambda (s)
             (remove-elements-from-set s singletons)) ls))) ;remove the singletons from each one of the sets in the current list

;; scans the whole matrix in the attempt of reducing
;; each set in each row to a list of singletons
(define (reduce-rows lls) 
  (cond[(resolved? lls) lls] ;if the matrix is solved return it
       [#t (map (lambda (ls) ;otherwise reduce each row
               (remove-native-singletons ls)) lls)])) 

;; transpose the matrix so that horizontal lists
;; become vertical and vice-versa
(define (rotate lls)
  (cond[(null? lls) (begin (printf "The matrix provided was empty\n") null)] ;safe check
       [#t (apply map list lls) ]))
  
;; given a list of sets and an element it returns true
;; if that element is not contained in any of the sets
;; in the list: otherwise it will return the element searched for
(define (unique? el ls)
  (if (null? ls)
      el ;return the element if we got to the end of the list with no failing tests!!
      (let([a-set (car ls)]
           [a-singleton (set el)]) ;make a singleton out of an element
        (cond[(subset? a-singleton a-set) #f]
             [#t (unique? el (cdr ls))]))))

;; replaces set1 in ls with set2
(define (replace set1 ls set2)
  (map (lambda (s)
         (if (set=? s set1)
             set2
             s)) ls))

;; attempts to solve the sudoku it will stop when it is solved
;; or no progress can be made...thunk the expression....
(define (start lls)
  (let* ([before lls]
         [before-count (count-all-singletons lls)]) ;count the total number of singletons before any processing is made
    (cond[(resolved? lls) (begin (printf "Solved! The solution is:\n") lls)] ; if its solved print it
         [(invalid-matrix? (resolve lls)) (begin (printf "This Sudoku cannot be solved. Current state:\n") before)] ;if we get duplicates we will return the previous matrix
         [(= before-count (count-all-singletons (resolve lls))) (begin (printf "Unable to proceed any further. Current state:\n") lls)] ;stop if no progress is made 
         [#t (start (resolve lls))]))) ;otherwise give it another go 

;; checks whether or not there is at least one row or column
;; with duplicate singleton sets
(define (any-row-with-dups lls)
  (cond[(> (length (filter has-duplicate-singleton? lls)) 0) #t] ;return true if at least  a row with duplicate sinlgeton is found
       [#t #f])) ;otherwise return false

;; checks whether a row contains duplicate singleton sets
(define (has-duplicate-singleton? ls)
  (check-duplicates ls (lambda (s1 s2)
                         (and (set=? s1 s2) (singleton? s1) (singleton? s2)))))

;; checks whether or not ther is a at least a 3 x 3 square
;; that contains two singleton sets that are the equal
(define (any-square-with-dups lls corners)
  (if (null? corners)
      #f
      (let* ([row (caar corners)]
             [col (last (car corners))]
             [square (get-square lls row col)]
             [singletons (get-singletons-in-matrix square)] ;get a list of all singletons
             [square-to-list (flat '() square)]) ;flatten the square
        (cond[(has-duplicate-singleton? square-to-list) #t] ;if the flattened square contains duplicates return true
             [#t (any-square-with-dups lls (cdr corners))])))) ;otherwise check the nexr square

;; launches the game performing an initial check to make sure it the matrix is valid
(define (solve lls)
  (if (invalid-matrix? lls)
      (error "Invalid start matrix: it cannot have duplicate elements in any row, column or 3 x 3 square. Fix it and try again.")
      (start lls)))

;; returns true if the matrix is valid, false otherwise
(define (invalid-matrix? lls)
  (or (any-row-with-dups lls) (any-row-with-dups (rotate lls)) (any-square-with-dups lls corners)))

;; processes the matrix until it is resolved
;; calling rotate & reduce rows in sequence translates to reduce columns
;; in order to always return the matrix in the original orientation
;; the rotatation must be happen twice: we have to reset the matrix
;; to the original orientation
(define (resolve lls)
  (cond[(resolved? lls) lls] ;stop if the matrix is resolved
       [#t ;otherwise process the matrix
        (rotate ; this call will reset the matrix
         (reduce-rows
          (rotate
           (solve-square
            (discover-singleton
             (reduce-rows lls)) corners))))]));

;; given a matrix it scan each row(column) looking
;; for potential: for each set in a row it will check whether or not there are elements
;; that don't appear in any other set in the same row(column). If any is found they will
;; be removed from the other set in the same row(column)
(define (discover-singleton lls)
  (map (lambda (ls)
         (let* ([singletons (extract-singletonfound (discover-singletonfound ls))]) ;; get the potential singletons
           (replace-set-with-singleton ls singletons))) lls))

;; given a list it replaces each set containing a newly discovered singleton with the singleton itself
(define (replace-set-with-singleton ls singletons)
  (cond[(null? singletons) ls] ;if there aren't any return the same list
       [#t (let* ([xx (car singletons)]) ;otherwise replace and call recursively
                      (replace-set-with-singleton (replace (singletonfound-set xx) ls (set (singletonfound-value xx))) (cdr singletons)))]))

;; given a list of elements it will return a list
;; of singletonfound structs if any were found in the inner lists
(define (extract-singletonfound lx)
  (let ([flx (flat '() lx)])
    (filter (lambda(x)
            (singletonfound? x)) flx)))

;; takes a list of sets and checks if any set has an element that does not appear any
;; other set in the list: if so it makes a "singletonfound" out of it and maps it to the set
(define (discover-singletonfound ls)
  (map (lambda(s)
            (let* ([a-list (remove s ls)] ;remove the current set from the list to get a sensible result
                   [slist (set->list s)]) ;turn the set into a list to scan through its elements easily
              (map (lambda (num) ;for each element in the set check it is the only one appearing in the list containing the set
                     (cond[(number? (unique? num a-list)) (singletonfound num s)] ; if so make a "singletonfound" type out of it and map it to the set
                          [#t #f] ;otherwise map the set to a false value
                          )) slist))) ls)) ;otherwise try again with the rest of the list

;; determines if the matrix has been resolved by checking if each
;; row contains only singleton sets
(define (resolved? lls)
  (cond [(= (length lls) (length (filter has-all-singleton lls))) #t]
        [#t #f]))

;; counts the total number of singletons in the matrix
(define (count-all-singletons lls)
  (foldl (lambda (ls total)
           (+ total (length ls))) 0 (map get-singletons-in-row lls)))

;; returns a list containing all singleton sets in the list
(define (get-singletons-in-row ls)
  (filter (lambda (s)
            (singleton? s)) ls))

;; given row and column coordinates returns the corresponding 3x3 square
;; NOTE: only valid coordinates (in the corners list) will return the sub-matrix
;; any other coordinate will cause an error to be thrown
(define (get-square lls row col)
  (let([rows (restrict-range lls row)])
    (map (lambda (ls)
              (restrict-range ls col)) rows)))

;; restricts the collection range based on the index provided  
;; NOTE: index-of does not work here, since sets with the same values are
;; considered equal if they occupy different positions in the list, they are considered the same
;; and index-of will return the first set having that index...to get around this
;; we are using first, second, third...functions
(define (restrict-range coll index)
  (cond[(= index 0)(list (first coll) (second coll) (third coll))]
       [(= index 3)(list (fourth coll) (fifth coll) (sixth coll))]
       [(= index 6) (list (seventh coll) (eighth coll) (ninth coll))]
       [#t (error "invalid index provided")]))

;; gets the square based on the coordinates find all the singletons in it
;; and removes the value enclised in each singleton from each other set in the (3 x 3) matrix
(define (process-square lls row col)
  (let* ([square (get-square lls row col)]
         [singletons (get-singletons-in-matrix square)])
    (map (lambda (ls)
           (remove-foreign-singletons ls singletons)) square)))

;; gets a list of all singletons in a matrix
(define (get-singletons-in-matrix lls)
  (let ([singletons (map (lambda (ls)
                           (get-singletons-in-row ls)) lls)])
        (flat '() singletons)))

;; NOTE: foreign-singletons (as opposed to native singletons)
;; stands for singleton that weren't necessarily found in the list itself)
;; This function removes a list of singletons from a list of sets
(define (remove-foreign-singletons ls singletons)
  (cond[(null? singletons) ls]
       [#t (remove-foreign-singletons (remove-subset-from-sets ls (car singletons)) (cdr singletons))]))

;; given a matrix and a set of coordinates it will try and solve each 3 x 3 square
;; indicated by the coordinates by reducing its sets to singletons
;; it will then replace the old squares with the new one
(define (solve-square lls corners)
  (cond[(null? corners) lls]
       [#t (let* ([row (caar corners)] ;get the row coordinate
                  [col (last (car corners))] ;get the column coordinate           
                  [new-square (process-square lls row col)] ;process the square corresponding to the coordinates
                  [new-matrix (do-replacement new-square lls row col)]) ; replace the old square with the new one
               (solve-square new-matrix (cdr corners)))])) ;keep doinig it recursively until we run out of coordinates
  
;; given a matrix and square it will replace the section indicated via coordinates
;; with the square provided
(define (do-replacement new-square lls row col)
  (map (lambda (ls)
         (cond[(= (index-of lls ls) row) (build-row ls (first new-square) col)]
              [(= (index-of lls ls) (+ row 1)) (build-row ls (second new-square) col)]
              [(= (index-of lls ls) (+ row 2)) (build-row ls (third new-square) col) ]
              [#t ls])) lls)) ;return the row as it was

;; it will build a list (row) containg a mix of the original row and the square-row passed as parameter
;; NOTE: col number can only be 0, 3, 6 and nothing else!!
(define (build-row ls square-row col)
  (cond[(= col 0)(flat '() (list square-row (cdddr ls)))] ;discard the first three
       [(= col 3)(flat '() (list (take ls 3) square-row (take-right ls 3)))] ;insert in between
       [(= col 6)(flat '() (list (take ls 6) square-row))] ;insert at the end
       [#t (error "invalid index provided")])) 
  
;; flat-map: flattens a list of list
(define (flat start nested)
  (cond[(null? nested) start] ;safe check
       [(null? (cdr nested)) (append start (car nested))]
       [#t (flat (append start (car nested)) (cdr nested))]))
  

  