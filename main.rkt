; Include the necessary files
(include "graphics.rkt")
(include "tiles.rkt")
(include "utils.rkt")
(include "game-logic.rkt")

; Wrapper for all the solver functions.
(define solve-board
  (lambda (board)
    (if(solved? board) ; Board in solved state? Don't bother doing anything with it
       board
    (begin ; else (Read down and up)
    (finalize-board						; Rotate one, three and four until board is solved
     (move-six-and-seven-into-place		; Move six and seven where they should be
      (move-six-into-waiting			; Move six to middle-left
       (adjust-for-fourth-stage			; Adjust board for next step
        (move-seven-into-waiting		; Move seven to bottom-left
         (move-two-and-five-into-place	; Move two and five where they should be
          (move-two-into-waiting 		; Move two to top-middle
           (adjust-for-third-stage 		; Adjust board for next step
            (move-five-into-waiting 	; Move five to top-right
             (adjust-for-second-stage 	; Adjust board for next step
              (move-eight-into-place 	; Move eight into bottom-right
               (adjust-initial-board-state board)))))))))))))))) ; Adjust initial board layout

(define play-me
  (lambda ()
    (sleep 2)
    (let ([shuffled-board (shuffle-board '(0 1 2 3 4 5 6 7 8) 1000)]) ; Shuffle initial board 1000 times
      (display "Solving board, hang on to your top hat")(newline) ; Display witticism.
      (print-line shuffled-board) ; Display the initial "seed"
      (sleep 2) ; Take a breather
      (solve-board (display-board shuffled-board))))) ; Now solve it!

(solved? (play-me))
; Seed (3 2 5 6 0 7 4 1 8) will force both 2/5 complex and 6/7 complex