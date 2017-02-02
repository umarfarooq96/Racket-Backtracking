#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-<)

; QUESTION 4
#|
(subsets lst)
  lst: a list

  A choice expression which yields a subset of 'lst'.
  Repeated calls to 'next' should yield *all* other subsets
  (i.e., 'next' returns "false." only when all subsets of 'lst'
  have been returned).

  The subsets can be yielded in any order; however, no subset
  can appear twice.

  Note that:
    - A subset isn't the same as a sublist. Items don't have to be consecutive.
    - A subset can be empty; the empty set is a subset of any list.
    - Order doesn't matter in subsets
  
  The following is an example of how 'subsets' is used.
  Note that your implementation might yield the subsets
  in a different order than the one shown here.

> (subsets '(1 2))
'()
> (next)
'(1)
> (next)
'(2)
> (next)
'(1 2)
> (next)
"false."
;subsets of '(1 2 3) gives '((), (1), (2), (2 1), (3), (1 3), (2 3), (1 2 3))
(-< (first ^))
Good way to think about subsets:
 subsets of s = either has some element in s or doesn't have it
 so subsets of (2 3) either have 1 ((1) (2 1) (1 3) (1 2 3)) or don't (() (2) (3) (2 3))
 subsets of 
|#
#|
(define (subsets s)
  (if (empty? s)
      (list s) ;if empty, return '(())
      (-< (let ([ps (subsets (rest s))]) ;let ps be the subset of (rest of s)
            (map (λ (l) (cons (first s) l)) ps)) ;now add first (like the 1) to each one
            
          (subsets (rest s)) ;still need the powersets without the first (without the 1)
          )))
|#
(define (build-subset s)
  (if (empty? s)
      (list s)
      (append (let ([ps (build-subset (rest s))])
                (map (λ (l) (cons (first s) l)) ps)) (build-subset (rest s)))))

(define (contpowerset lst)
  (if (empty? (rest lst))
      empty
      (-< (first lst) (contpowerset (rest lst)))
      ))

(define (subsets s)
  (let ([subs (build-subset s)])
    (contpowerset subs))
  )

;(all (subsets '(1 2 3 4 5)))
; QUESTION 5
#|
(sudoku-4 puzzle)
  puzzle: a nested list representing a 4-by-4 Sudoku puzzle

  A choice expression that represents possible solutions to the puzzle.
  Upon evaluation, just one solution is returned, but repeated calls
  to 'next' produces more possible solutions, if any.

  Hint: use the ?- function in your solution. Again, your main task
  is just to correctly express the constraints, and let the computer
  do the work.

?- used like this: (?- (helper that checks if sudoku solution is valid) (get all sudoku possibilites))

Some thoughts:
?- will 

|#
(define sudoku-4 (void))

;given ("" "" 2 3)
;(define (insert lst val)
;  (if (empty? lst)
;      (list val)
;      (-< (cons val lst)
;          (cons (first lst)
;                (insert (rest lst) val)))))
;
;(define (permutation lst)
;  (if (empty? lst)
;      '()
;      (insert (permutation (rest lst))
;              (first lst))))
;
;;(?- (λ (lst) (> (first lst) 3))
;    (permutation '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20));)


(define (setQuoteToOne lst)
  (reverse (setQuoteToOne-helper lst '())))

(define (setQuoteToOne-helper lst acc)
  (if (empty? lst) acc
      (if (equal? (first lst) "") (setQuoteToOne-helper (rest lst) (cons 1 acc))
          (setQuoteToOne-helper (rest lst) (cons (first lst) acc)))))


(define (quoteIndecies lst)
  (reverse (quoteIndecies-helper lst '() 0)))

(define (quoteIndecies-helper lst acc n)
  (if (empty? lst) acc
      (if (equal? (first lst) "") (quoteIndecies-helper (rest lst) (cons n acc) (+ n 1))
          (quoteIndecies-helper (rest lst) acc (+ n 1)))))

;(incrementIndex '(1 1 2 3) 3) -> (1 1 2 4)
;(quoteIndecies '("" "")) -> (0 1)


#|
(1 1)(1 2)(1 3)(1 4)(2 1)(2 2)(2 3)(2 4)(3 1)...
Increment rest.
|#
;(define (get-possible puzzle idx)
;  (incrementIndex puzzle (first idx)))

#|
A valid sudoku puzzle is valid iff it's rows, columns and cells are valid.
Valididity is given in the handout.

(valid-sudoku-4 ('((1 2 3 4)(3 4 1 2)(4 1 2 3)(2 3 4 1))) gives #t
(valid-sudoku-4 ('((1 2 3 4)(1 4 1 2)(4 1 2 3)(2 3 4 1))) gives #f
|#
(define (valid-sudoku-4 puzzle)
  (and (valid-rows puzzle) (valid-columns puzzle) (valid-cells puzzle)))

;just check if first is in rest. if it is member, then same number appeared twice = #f.
(define (valid-row row)
  (cond [(empty? row) #t]
        [(member (first row) (rest row)) #f]
        [else (valid-row (rest row))]))

(define (valid-rows puzzle)
  (cond [(empty? puzzle) #t]
        [(not (valid-row (first puzzle))) #f] ;if a row isn't valid, puzzle isn't.
        [else (valid-rows (rest puzzle))]))

;if any column is an invalid row (list with dups), then false.
(define (valid-columns puzzle)
  (cond [(empty? (first puzzle)) #t]
        [(valid-row (get-column puzzle))]
        [else (valid-row (get-column (delete-first-column puzzle)))]
        ))

(define (valid-cells puzzle)
  (valid-rows (get-cells puzzle)))

;Couple Helpers. No point being cute with these, know it's a 4x4.
(define (get-cells puzzle)
  (list (list (first (first puzzle)) (second (first puzzle))
              (first (second puzzle)) (second (second puzzle)))
        (list (third (first puzzle)) (fourth (first puzzle))
              (third (second puzzle)) (fourth (second puzzle)))
        (list (first (third puzzle)) (second (third puzzle))
              (first (fourth puzzle)) (second (fourth puzzle)))
        (list (third (third puzzle)) (fourth (third puzzle))
              (third (fourth puzzle)) (fourth (fourth puzzle)))))

(define (delete-first-column puzzle)
  (list (rest (first puzzle))
        (rest (second puzzle))
        (rest (third puzzle))
        (rest (fourth puzzle))))
(define (get-column puzzle)
  (list (first (first puzzle))
        (first (second puzzle))
        (first (third puzzle))
        (first (fourth puzzle))))

; QUESTION 6
#|
(fold-< combine init expr)
  combine: a binary function
  init: an initial value
  expr: a choice expression

  Evaluate all choices in <expr> and combine them, one at a time, with the
  initial value, and return the result.

  Note that the order of <combine>'s parameters is the same as foldl:
    1) The value of the next choice
    2) The value of <init>
|#
(define-syntax fold-<
  (syntax-rules ()
    [(fold-< combine init expr)
     (let* (
            [val init]
            [fold-helper
             (λ (e)
               (set! val (combine e val))
               (next)
               val)])
       (fold-helper expr))]
    
    
    ))