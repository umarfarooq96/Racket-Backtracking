#| Choice Implementation

This is a cleaned-up version of the code from lecture.
While you are responsible for both understanding the
implementation and public API, only the latter is
required for this assignment.

We strongly recommend not changing this file.
|#
#lang racket
(provide -< next)

#|
(-< <expr> ...)
  Each <expr> is an arbitrary Racket expression.

  Evaluates and returns the first <expr>.
  If there is more than one argument, stores a choice point
  which resumes the program at where the (-< ...) is used,
  but with the remaining choices.
|#
(define-syntax -<
  (syntax-rules ()
    ; When there is only one option, return it.
    [(-< <expr1>) (begin
                    (remove-peek! (quote (-< <expr1>))) 
                    <expr1>)]
    ; When there is more than one, return the first and store the rest.
    [(-< <expr1> <expr2> ...)
     (begin
       (remove-peek! (quote (-< <expr1>))) 
       (add-peek! (quote (-< <expr2> ...)))
       (let/cc cont
       ; Push a new choice onto choices.
       (add-choice! (lambda () (cont (-< <expr2> ...))))
       <expr1>))]))


#|
(next)

  Backtracks to the most recently stored choice point and
  resume program execution from there, or returns "false."
  if there are no choice points stored.
> (-< 1 2 3)
1
> (next)
2
> (next)
3
> (next)
"false."
|#
(define (next)
  ; Check if there are any remaining choices
  (if (empty? choices)
      "false."
      ; Notice that it's ((get-choice!)) and not (get-choice!).
      ; What's the difference?
      (begin
      ((get-choice!))
      )))

;------------------------------------------------------------------------------
; Private values for managing the stack of choices.
;------------------------------------------------------------------------------

; The stack of choice points, represented as a list.
(define choices '())

; "Push": add a choice to the choices stack.
(define (add-choice! choice)
  (set! choices
        (cons choice choices)))

; "Pop": remove and return first choice from
; the choices stack.
(define (get-choice!)
  (let ([choice (first choices)])
    (set! choices (rest choices))
    choice))

#|
---------------------------------------------------------------
 PEEK EXPLANATION
---------------------------------------------------------------
~~Changes~~

Made a peek "data structure" as defined below. Unlike the (next),
we don't want to "get" the first peek, we just want to see it.
When we actually do it (in macro) then have reason to remove it.

tl;dr (peek) doesn't consume

In macro:
For (-< <expr1>), remove from peek and do <expr1>.

    (-< <expr1> <expr2> ...),
      we're "doing" <expr1> so can remove that. Next to do is
      <expr2> and onwards (the ...) so save (-< <expr2> ...).
  
|#
(define peekchoice '())

(define (add-peek! choice)
  (set! peekchoice
        (cons choice peekchoice)))

(define (remove-peek! x)
  (set! peekchoice (remove x peekchoice)))

(define (see-peek!)
  (first peekchoice))

(define (peek)
 (if (empty? choices)
      "false."
      (see-peek!)))

;~~ TESTING ~~
;(-< 1 2 3)
;(peek)
;(next)
;(peek)
;(next)
;(peek)
;(+ (-< 1 2) (-< 3 10)) ;gives 4 by doing (-< 1 2): (λ (x) (+ x (-< 3 10))). and getting the (-< 3)
;(peek) ;should be (-< 10) -- got the 3, so next is (-< 10)
;(next) ;gives 11 
;(peek) ;should be (-< 2) -- now done with 1 so time to move to (-< 2)
;(next) ;gives 5 
;(peek) ;should be (-< 10) -- got the 3, so next is (-< 10)
;(next) ;gives 12
;(peek) ;should be "false".
;(next) ;gives false nothing left
