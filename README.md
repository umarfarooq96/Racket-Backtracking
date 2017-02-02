# Racket-Backtracking

1. Using backtracking repeatedly generate all subsets of a list by calling (next).

2. A 4-by-4 Soduko solver. Calling (next) will return a new solution. 

3. A generalized (foldl) call that uses backtracking. Example,

    (fold-< max 0 (sin (* (-< 1 2 3 4) (+ (-< 100 200) (-< 1 2)))))
    
    0.9948267913584064
    
4. A peek implementation to see the next choice in a quote representation. 
