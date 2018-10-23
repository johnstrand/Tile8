; Functions for checking what moves are possible with specified board state?
(define can-move-left?
  (lambda (board)
    (< (get-x-of-number board 0) 2)))

(define can-move-down?
  (lambda (board)
    (> (get-y-of-number board 0) 0)))

(define can-move-right?
  (lambda (board)
    (> (get-x-of-number board 0) 0)))

(define can-move-up?
  (lambda (board)
    (< (get-y-of-number board 0) 2)))

; Wrapper for the four above functions
(define can-move-direction?
  (lambda (board dir)
    (cond
      ((equal? dir "up") (can-move-up? board))
      ((equal? dir "down") (can-move-down? board))
      ((equal? dir "left") (can-move-left? board))
      ((equal? dir "right") (can-move-right? board))
      (else #f))))

; Is it possible to move the center piece?
(define can-center-move?
  (lambda (board)
    (or
     (number-is-at-index? board 0 1)
     (number-is-at-index? board 0 3)
     (number-is-at-index? board 0 5)
     (number-is-at-index? board 0 7))))
	 ; Yes, if any of of these indices are empty

; Determines which moves are possible at current board state
(define get-possible-directions
  (lambda (board dir-list)
    (if(null? dir-list) ; All directions tested, return empty list
       null
       (begin
         (let ([current-dir (car dir-list)]) ; Grab the first move from the list
           (cond ; Test whether the move matches an acceptable move, and if the move is possible. If so, add it to the resulting list.
             ((and (equal? current-dir "up") (can-move-up? board)) (cons current-dir (get-possible-directions board (cdr dir-list))))
             ((and (equal? current-dir "down") (can-move-down? board)) (cons current-dir (get-possible-directions board (cdr dir-list))))
             ((and (equal? current-dir "left") (can-move-left? board)) (cons current-dir (get-possible-directions board (cdr dir-list))))
             ((and (equal? current-dir "right") (can-move-right? board)) (cons current-dir (get-possible-directions board (cdr dir-list))))
             (else (get-possible-directions board (cdr dir-list)))))))))

; These functions finds the index above, below, to the left of and to the right of the empty tile
(define get-above-index
  (lambda (board)
    (+ (* (- (get-y-of-number board 0) 1) 3) (get-x-of-number board 0))))

(define get-below-index
  (lambda (board)
    (+ (* (+ (get-y-of-number board 0) 1) 3) (get-x-of-number board 0))))

(define get-left-index
  (lambda (board)
    (+ (* (get-y-of-number board 0) 3) (+ (get-x-of-number board 0) 1))))

(define get-right-index
  (lambda (board)
    (+ (* (get-y-of-number board 0) 3) (- (get-x-of-number board 0) 1))))

; Wrapper for above functions, makes it easy to find a specific tile in relation
; to the empty tile
(define get-index-from-dir
  (lambda (board dir)
    (cond
      ((equal? dir "up") (get-below-index board))
      ((equal? dir "down") (get-above-index board))
      ((equal? dir "left") (get-left-index board))
      ((equal? dir "right") (get-right-index board))
      (else -1))))

; Checks whether or not the empty tile is in the middle
(define middle-is-empty?
  (lambda (board)
    (number-is-at-index? board 0 4)))

; Checks whether or not a given tile is at a given index
(define number-is-at-index?
  (lambda (board number index)
    (= (get-index-of-number board number) index)))

; Checks if the board is in a good state to start solving, 
; i.e., neither the empty tile nor the 8-tile is in the middle. 
(define board-in-good-state?
  (lambda (board)
    (let ([index-of-eight (get-index-of-number board 8)]
          [index-of-empty (get-index-of-number board 0)])
      (and
       (not (= index-of-eight 4))
       (not (= index-of-empty 4))))))

; Executes a series of moves, recursing over a list of moves and a changing board
; and returns the final board state
(define moves
  (lambda (board dirs)
    (if(or (null? dirs) (solved? board))
       board
       (moves (move-with-delay board (car dirs)) (cdr dirs)))))

; Performs a certain move with a delay for animation
(define move-with-delay
  (lambda (board dir)
    (if (can-move-direction? board dir) ; Is move legal?
        (begin
          (draw-move board dir) ; Animate the move
          (move board dir)) ; Perform the move and return new board state
        board))) ; Illegal move request, return board unchanged

(define move
  (lambda (board dir)
    (yield) ; Let the UI catch up
    (if (can-move-direction? board dir) ; Is move legal?
        (begin
          (let ([empty-index (get-index-of-number board 0)] ; Grab the two indices, and swap their values
                [source-index (get-index-from-dir board dir)])
              (swap-indexes board empty-index source-index))) ; Return new board state
        board))) ; Illegal move request, return board unchanged