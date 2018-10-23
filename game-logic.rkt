; This function will shuffle the board to be solved
(define shuffle-board
 (lambda (board n)
     (if (zero? n) ; If n is zero, return the finalized board
         board
         (begin
           (yield) ; Let the UI catch up
           (shuffle-board ; Recurse on the new board state and n - 1
             (move board ; Perform one random move out of the possible moves in the list
                   (get-random 
                    (get-possible-directions board 
                                             (list "up" "down" "left" "right")))) (- n 1))))))

; This function will perform initial adjustments to make sure that the board is
; in a good state to be solved
(define adjust-initial-board-state
  (lambda (board)
    (cond
      ((solved? board) board) ; Is it solved or in a good state already? Return it as-is
      ((board-in-good-state? board) board)
      ((middle-is-empty? board) (adjust-initial-board-state (occupy-middle board))) ; Is the empty tile in the middle? Move something in there
      ((eight-is-in-the-middle? board) (adjust-initial-board-state (clear-middle board))) ; Is the eight in the middle? Move it out and recurse on the new board state
      (else board)))) 
	  ; Nothing to do, just return (this should never happen, so I'm not sure why his is still here.

; This function will move out the center tile
(define clear-middle
  (lambda (board)
    (if (can-center-move? board) ; Can we move?
        (begin
          (let ([zero-index (get-index-of-number board 0)]) ; Get index of empty tile
            (swap-indexes board zero-index 4))) ; Swap the middle index and the index for the empty tile
        (external-rotate board 0 1)))) ; Perform an external rotate until the empty tile is in the top middle
 
; An external rotate refers to "moving" the empty tile around the outside of the board until a certain
; value has reached a certain index
(define external-rotate
  (lambda (board value index)
    (if (or (number-is-at-index? board value index) (solved? board)) ; Are we in the right place or is the board in solved state?
          board ; Return current state
          (external-rotate ; otherwise, perform a move sequence and iterate on the resulting board state
           (move-sequence board value index (list "down" "down" "right" "right" "up" "up" "left" "left")) value index))))

; A vertical rotate refers to "moving" the empty tile in the sequence 0 1 4 7 6 3, or reverse until a
; certain value has reached a certain index.
(define vertical-rotate
  (lambda (board value index)
    (if (or (number-is-at-index? board value index) (solved? board)) ; Are we in the right place or is the board in solved state?
        board ; Return current state
        (vertical-rotate ; otherwise, perform a move sequence and iterate on the resulting board state
         (move-sequence board value index (list "down" "down" "right" "up" "up" "left")) value index))))

; A horizontal rotate refers to "moving" the empty tile in the sequence 0 1 2 5 4 3, or reverse until a
; certain value has reached a certain index
(define horizontal-rotate
  (lambda (board value index)
    (if (or (number-is-at-index? board value index) (solved? board)) ; Are we in the right place or is the board in solved state?
        board ; Return current state
        (horizontal-rotate ; otherwise, perform a move sequence and iterate on the resulting board state
         (move-sequence board value index (list "right" "right" "down" "left" "left" "up")) value index))))

; A square rotate refers to "moving" the empty tile in the sequence 0 1 4 3, or revers until a certain
; value has reached a certain index
(define square-rotate
  (lambda (board value index)
    (if (or (number-is-at-index? board value index) (solved? board)) ; Are we in the right place or is the board in solved state?
        board ; Return current state
        (square-rotate ; otherwise, perform a move sequence and iterate on the resulting board state
         (move-sequence board value index (list "down" "right" "up" "left")) value index))))

; Will perform a series of moves until a condition is met, and return the final board state
(define move-sequence
  (lambda (board value index sequence)
    (define new-board (sequencer board value index sequence)) ; Perform the specified series of moves
    (if (or (number-is-at-index? new-board value index) (solved? new-board)) ; Is the condition met or is the board solved
          new-board ; Return it
          (move-sequence new-board value index sequence)))) ; otherwise, recurse on the board state

; Performs a series of moves on the board until it has reached a certain state.
(define sequencer
  (lambda (board value index sequence)
    (cond
      ((solved? board) board) ; Board solved, just return it
      ((null? sequence) board) ; Out of moves to perform, return board in current state
      ((number-is-at-index? board value index) board) ; Condition met? Return board.
      (else
       (let ([dir (car sequence)]) ; Get the next move
         (sequencer (move-with-delay board dir) value index (cdr sequence))))))) ; Perform move on board, recurse on board and move sequence

; Moves a tile that isn't 8 into the middle
(define occupy-middle
  (lambda (board)
    (if (number-is-at-index? board 8 1) ; Is eight in top-middle?
        (move-with-delay board "left") ; Yes: Move left
        (move-with-delay board "down")))) ; No: Move down

; Checks whether the center tile is the 8
(define eight-is-in-the-middle?
  (lambda (board)
    (number-is-at-index? board 8 4)))

; Moves the 8 into place at bottom-right
(define move-eight-into-place
  (lambda (board)
    (print-line "Moving 8 into place")
    (external-rotate board 8 8)))

; Moves 5 into waiting place by observing the following rules:
; 5 is at index 2 (i.e., waiting place): Return board as is.
; 5 is at index >5 (i.e., bottom row): Perform vertical rectangle until it is as index 1, then recurse on board state
; else: Perform horizontal rectangle until 5 is at index 2.
(define move-five-into-waiting
  (lambda (board)
    (print-line "Moving 5 into waiting")
    (cond
      ((= (get-index-of-number board 5) 2) board)
      ((> (get-index-of-number board 5) 5) (move-five-into-waiting (vertical-rotate board 5 1)))
      (else (horizontal-rotate board 5 2)))))

; Moves 2 into waiting place by performing a vertical rotate until it reaches index 1	  
(define move-two-into-waiting
  (lambda (board)
    (print-line "Moving 2 into waiting")
    (if(= (get-index-of-number board 2) 1)
       board
       (vertical-rotate (move board "right") 2 1))))

; Moves 2 and 5 into their final position by performing a horizontal rotate until 5 reaches index 5
(define move-two-and-five-into-place
  (lambda (board)
    (print-line "Moving 2 & 5 into place")
    (horizontal-rotate board 5 5)))

; Moves 7 into waiting position by performing a vertical rotate until 7 is at index 6
(define move-seven-into-waiting
  (lambda (board)
    (print-line "Moving 7 into waiting")
    (if(= (get-index-of-number board 7) 6)
       board
       (vertical-rotate board 7 6))))

; Moves 7 into waiting position by performing a square rotate until 6 is at index 3
(define move-six-into-waiting
  (lambda (board)
    (print-line "Moving 6 into waiting")
    (if(= (get-index-of-number board 6) 6)
       board
       (square-rotate (move board "down") 6 3))))

; Moves 6 and 6 into their final positions by performing a vertical rotate until 7 reaches index 7
(define move-six-and-seven-into-place
  (lambda (board)
    (print-line "Moving 6 & 7 into place")
    (vertical-rotate board 7 7)))

; Sometimes, 2 and 5 will be right next to each other, but in wrong order. In this case, we perform
; a complex, which is a series of non-repeating movements in order to separate the two.
(define two-five-complex
  (lambda (board)
    (print-line "Performing 2/5 complex")
    (move-five-into-waiting (moves board (list "down" "right" "up")))))

; See above
(define six-seven-complex
  (lambda (board)
    (print-line "Performing 6/7 complex")
    (move-seven-into-waiting (moves board (list "right" "up" "left" "down" "right")))))

; Perform intermediary adjustments between stage 1 and 2
(define adjust-for-second-stage
  (lambda (board)
    (print-line "Adjusting for second stage")
    (move-with-delay board "right")))

; Perform intermediary adjustments between stage 2 and 3
(define adjust-for-third-stage
  (lambda (board)
    (print-line "Adjusting for third stage")
    (if (= (get-index-of-number board 2) 4)
        (two-five-complex board)
        (move-with-delay board "right"))))

; Perform intermediary adjustments between stage 3 and 4
(define adjust-for-fourth-stage
  (lambda (board)
    (print-line "Adjusting for fourth stage")
    (cond
      ((= (get-index-of-number board 6) 7) (six-seven-complex board))
      ((= (get-index-of-number board 6) 4) (six-seven-complex (move board "down")))
      (else (move-with-delay board "down")))))

; Perform final adjustments n times, if we fail to solve the board before n hits zero, reshuffle the
; board and start from the top (this should not happen).
(define final-adjustments
  (lambda (board adjustments n)
    (cond
      ((solved? board) board)
      ((= n 0)(display-line "Re-adjusting") (solve-board (shuffle-board board) 100))
      ((null? adjustments) (final-adjustments board (list "down" "right" "up" "left") (- n 1)))
      (else (final-adjustments (move-with-delay board (car adjustments)) (cdr adjustments) (- n 1))))))

; Attempt to finalize the board over 20 moves.
(define finalize-board
  (lambda (board)
    (print-line "Performing final adjustments")
    (final-adjustments board '() 20)))