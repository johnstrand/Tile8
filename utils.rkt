; Returns a list of x and y coordinates for the specified tile number
(define get-coords-of-number
  (lambda (board n)
    (list (get-x-of-number board n) (get-y-of-number board n))))

; Returns the x-position of a given index
(define get-x-of-index
  (lambda (index)
    (remainder index 3)))

; Returns the y-position of a given index
(define get-y-of-index
  (lambda (index)
    (floor (/ index 3))))

; Returns the x-position of a given tile number
(define get-x-of-number
  (lambda (board n)
    (remainder (get-index-of-number board n) 3)))

; Returns the y-position of a given tile number
(define get-y-of-number
  (lambda (board n)
    (floor (/ (get-index-of-number board n) 3))))

; Returns the tile number corresponding to a given index
(define get-number-by-index
  (lambda (board n)
    (cond
      ((zero? n) (car board))
      (else (get-number-by-index (cdr board) (- n 1))))))

; Returns a tile number that corresponds with a given position
(define get-number-by-coords
  (lambda (board x y)
    (get-number-by-index board (get-index-from-coords x y))))

; Returns an index from a given tile number
(define get-index-of-number
  (lambda (board n)
    (match-number-to-index board n 0)))

; Checkes whether a given tile number is at a given index
(define match-number-to-index
  (lambda (board n index)
    (cond
      ((= (car board) n) index)
      (else (match-number-to-index (cdr board) n (+ index 1))))))

; Convert position to index
(define get-index-from-coords
  (lambda (x y)
    (+ (* y 3) x)))

; Returns a number tile by index
(define get-by-index
  (lambda (ls n)
    (cond
      ((null? ls) #f)
      ((zero? n) (car ls))
      (else (get-by-index (cdr ls) (- n 1))))))

; Sets a specific index to a specific tile
(define set-by-index
  (lambda (ls n v)
    (cond
      ((null? ls) null)
      ((zero? n) (cons v (cdr ls)))
      (else (cons (car ls) (set-by-index (cdr ls) (- n 1) v))))))

; Get a random element from a list
(define get-random
  (lambda (ls)
    (get-by-index ls (random (length ls)))))

; Swap the values of two indices
(define swap-indexes
  (lambda (ls i1 i2)
    (let ([v1 (get-by-index ls i1)]
          [v2 (get-by-index ls i2)])
      (display-board (set-by-index (set-by-index ls i1 v2) i2 v1)))))

; Prints a line followed by a newline
(define print-line
  (lambda (line)
    (display line)(newline)
    line))

; Checkes whether a puzzle is solved
(define solved?
  (lambda (board)
    (count board 0)))

; Counts upwards, testing so that tile 0 is at index 0, tile 1 is at index 1, ..., tile N is at index N.
(define count
  (lambda (board n)
    (cond
      ((null? board) #t)
      ((equal? (car board) n)(count (cdr board) (+ n 1)))
      (else #f))))