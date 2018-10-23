(define limit-x 360) ; Size in width and height of the window
(define limit-y 380) ; Height is a bit bigger to compensate for space lost to the window title bar

; Load the tiles, this could have been done recursively, but I feel that this gives a better overview
; of what's going on.
(define tiles (list 
               (read-bitmap "tile0.png")
               (read-bitmap "tile1.png")
               (read-bitmap "tile2.png")
               (read-bitmap "tile3.png")
               (read-bitmap "tile4.png")
               (read-bitmap "tile5.png")
               (read-bitmap "tile6.png")
               (read-bitmap "tile7.png")
               (read-bitmap "tile8.png")))

(define get-pen ; Creates a pen of width 1 with specified color
  (lambda (pen-color)
    (new pen% [color pen-color] [width 1]))) 

(define plot
  (lambda (dc x y plot-color)
    (define pen (get-pen plot-color)) ; Create a pen of specified color
    (send dc set-pen pen)
    (send dc draw-line x y x y))) ; Draws a line onto specified Device Context. This line has the same start and end, giving a single pixel.

(define draw-number
  (lambda (dc x y number)
    (if(< 0 number) ; Remember kids, the zero is an empty block
       (begin
         (send dc set-pen (get-pen "black")) ; Set current color to black
         (send dc draw-text (number->string number) x y))))) ; Convert number to string and draw it to sceen

(define draw-move
  (lambda (board dir)
    (let* ([empty-index (get-index-of-number board 0)]		; Index of the empty tile
           [source-index (get-index-from-dir board dir)]	; Index of the tile we're moving
           [tx (* (get-x-of-index empty-index) 120)]		; Target X
           [ty (* (get-y-of-index empty-index) 120)]		; Target Y
           [sx (* (get-x-of-index source-index) 120)]		; Source X
           [sy (* (get-y-of-index source-index) 120)])		; Source Y;
      (if (and (< -1 sx) (< -1 sy) (< -1 tx) (< -1 ty))		; If all are 0 or higher
          (begin
            (let ([dx (- tx sx)]							; Delta X
                  [dy (- ty sy)])							; Delta Y
              (draw-transition sx sy dx dy (get-by-index board source-index) 0)))))))
			  ; Perform the actual drawing of the transition

(define draw-transition
  (lambda (source-x source-y delta-x delta-y number frac)
    (if (< frac 1.1) ; If frac is in the range 0 - 1
        (begin
          (yield) ; Let the UI catch up
		  ; The position in this case is going to be the position the tile originally had, plus
		  ; the difference between the original position and the final position multiplied by a value in the range 0 - 1
          (draw-image-by-number (+ source-x (* delta-x frac)) (+ source-y (* delta-y frac)) number)
          (sleep 0.05) ; Wait 0.05 seconds
          (draw-transition source-x source-y delta-x delta-y number (+ frac 0.2)))))) ; Increase delta multiplier value

; Grabs a tile corresponding to a number and draws it at the specified position
(define draw-image-by-number
  (lambda (x y number)
    (send dc draw-bitmap (get-by-index tiles number) x y)))

; Draw the entire board, accepts two copies of the board, one to use for position lookups
; and one to use for drawing.
(define draw-board
  (lambda (board ls)
    (if(null? ls) ; All done plotting, return board
       board
       (begin
         (let ([num (car ls)]) ; Get the current number
           (let ([x (* 120 (get-x-of-number board num))] ; Get the position corresponding to the current number
                 [y (* 120 (get-y-of-number board num))])
             (draw-image-by-number x y num) ; Draw the number
             (draw-board board (cdr ls)))))))) ; Move on to the next number
         
(define display-board
  (lambda (board)
    (yield) ; Give the UI a chance to catch up
    (draw-board board board))) ; Draw the board and return it, unchanged


; Create the main window with necessary dimensions
(define frame (new frame%
                   [label "Sliding puzzle"]
                   [width limit-x]
                   [height limit-y]))

; Create a canvas to draw on, leave the paint-callback empty as we will be supplying our own.
(define c (new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                null)]))

(send frame show #t)
(define dc (send c get-dc))