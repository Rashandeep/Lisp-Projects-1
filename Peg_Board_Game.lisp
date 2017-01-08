; Program to calculate a solution to the Peg Board Game
;
;          o
;         T T
;        T T T
;      T  T  T T
;     T  T  T  T T

; A board is represented as a list rows, each row is a
; list of positions (peg_pos), each of which is either T or nil.
; A board with five rows is
;
;   ((T T T T T)(T T T T)(T T T)(T T)(NIL))

; Each hole on the board is defined as a peg_pos (row peg)
; A move is defined as from peg_pos to peg_pos)


; ----------------------------------------------------
; Main search routines
; ----------------------------------------------------
; Function test_peg_board
(defun test_peg_board (board_size)
  (let ((board (make_board board_size))
        (solution nil))
    (if (setf solution (find_peg_board_solution board))
      (print_solution (make_board 5) solution)
      (print "No solution."))))

; Function find_peg_board_solution
(defun find_peg_board_solution (board)
  (let ((first_moves nil)
        (rest_of_moves nil))

    (setf first_moves (find_peg_board_moves board)) ; Get top level moves
    (if (null first_moves)

      (if (eql (num_pegs board) 1)
        (block success              ; Only one peg left
          (list 'done))
        (block dead_end             ; more than one peg, no moves
          nil))

      (do ((move (pop first_moves)(pop first_moves)))  ; For each top level move
           ((null move) nil)

        (move_peg board (car move) (cadr move))
        (setf rest_of_moves (find_peg_board_solution board))  ; find solution
        (if (consp rest_of_moves)
          (block success2
            (push move rest_of_moves)
            (return-from find_peg_board_solution rest_of_moves))
          (block dead_end2
            (unmove_peg board (car move) (cadr move))
            nil))))))

; Function find_peg_board_moves
; Iterates through each peg_hole in the board and collects
; into one flat list all of the moves available.
(defun find_peg_board_moves (board)
  (let ((len (length board))
        (peg_moves nil)
        (moves nil))
    (do ((row 0 (+ row 1)))
        ((>= row len) moves)
      (do ((pos 0 (+ pos 1)))
          ((>= pos (- len row)) moves)
        (setf peg_moves (find_moves_for_peg board (list row pos)))
         (if peg_moves (mapcar #'(lambda(m) (push m moves)) peg_moves))))))

; Function find_moves_for_peg
; Takes as input a board and the position of a peg
; Provides as output a list of potential moves
(defun find_moves_for_peg (board peg_pos)
    (if (is_filled_peg_pos board peg_pos)
      (let ((moves nil))
        (if (and
              (is_filled_peg_pos board (vector_peg_pos 'ul 1 peg_pos))
              (is_empty_peg_pos board (vector_peg_pos 'ul 2 peg_pos)))
            (push (list peg_pos 'ul) moves))
        (if (and
            (is_filled_peg_pos board (vector_peg_pos 'ur 1 peg_pos))
            (is_empty_peg_pos board (vector_peg_pos 'ur 2 peg_pos)))
            (push (list peg_pos 'ur) moves))
        (if (and
            (is_filled_peg_pos board (vector_peg_pos 'r 1 peg_pos))
            (is_empty_peg_pos board (vector_peg_pos 'r 2 peg_pos)))
            (push (list peg_pos 'r) moves))
        (if (and
            (is_filled_peg_pos board (vector_peg_pos 'dr 1 peg_pos))
            (is_empty_peg_pos board (vector_peg_pos 'dr 2 peg_pos)))
            (push (list peg_pos 'dr) moves))
        (if (and
            (is_filled_peg_pos board (vector_peg_pos 'dl 1 peg_pos))
            (is_empty_peg_pos board (vector_peg_pos 'dl 2 peg_pos)))
            (push (list peg_pos 'dl) moves))
        (if (and
            (is_filled_peg_pos board (vector_peg_pos 'l 1 peg_pos))
            (is_empty_peg_pos board (vector_peg_pos 'l 2 peg_pos)))
            (push (list peg_pos 'l) moves))
        moves)))

; ----------------------------------------------------
; board utilities
; ----------------------------------------------------

; Function: print_solution
(defun print_solution (board solution)
  (format t "Starting board: ~%")
  (print_board board)
  (do ((move (pop solution)(pop solution)))
      ((or (null move)(eql move 'done)) move)
    (format t "Making move: ~A ~%" move)
    (move_peg board (car move)(cadr move))
    (print_board board)))

; Function: make_board
; Takes as input the number of rows n in the board
; Creates a board with the single hole in the top row
; empty and the remaining (n-1) rows filled with pegs
(defun make_board (n)
  (if (eq n 1)
    (list (list nil))
    (cons (make_full_row n)
          (make_board (- n 1)))))

; Function make_full_row
(defun make_full_row  (n)
  (if (eq n 1)
    (list T)
    (cons T
          (make_full_row (- n 1)))))

; Function: print_board
(defun print_board (board)
  (let ((board_len (length board))
        (rboard (reverse board)))

   (mapcar
      #'(lambda (row)
          (print_row row board_len))
      rboard)
    (format t "~%")))

; Function: print_row
(defun print_row (row board_len)
  (pprint-tab :line (round (* (- board_len (length row)) 2)) 1)
  (mapcar
    #'(lambda (peg_hole)
        (if peg_hole (format t "  X ")
          (format t "  0 ")))
    row)
    (format t "~%"))




; Function: num_pegs
(defun num_pegs (board)
  (let ((len (length board))
        (peg_count 0))
    (do ((row 0 (+ row 1)))
        ((>= row len) peg_count)
      (do ((pos 0 (+ pos 1)))
          ((>= pos (- len row)) peg_count)
        (if (is_filled_peg_pos board (list row pos))
          (setf peg_count (+ 1 peg_count)))))
    peg_count))

; ----------------------------------------------------
; peg position utilities
; ----------------------------------------------------

; Function move_peg
; Moves a peg from a peg_pos in a direction
; Does not error check
(defun move_peg (board peg_pos dir)
  (set_peg_pos board peg_pos nil)
  (set_peg_pos board (vector_peg_pos dir 2 peg_pos) T)
  (set_peg_pos board (vector_peg_pos dir 1 peg_pos) nil)
  board)

; Function unmove_peg
; Moves a peg from a peg_pos in a direction
; Does not error check
(defun unmove_peg (board peg_pos dir)
  (set_peg_pos board peg_pos T)
  (set_peg_pos board (vector_peg_pos dir 2 peg_pos) nil)
  (set_peg_pos board (vector_peg_pos dir 1 peg_pos) T)
  board)

; Function vector_peg_pos
; Given a peg position, returns a peg position shifted
; a length in a given direction.  Does not check whether the
; returned position is valid.
(defun vector_peg_pos (dir len peg_pos)
  (let ((up 0)(right 0))
    (cond ((eql dir 'l)(setf up 0)(setf right ( * -1 len)))
          ((eql dir 'ul)(setf up len)(setf right (* -1 len)))
          ((eql dir 'ur)(setf up len)(setf right 0))
          ((eql dir 'r)(setf up 0)(setf right len))
          ((eql dir 'dr)(setf up ( * -1 len))(setf right len))
          ((eql dir 'dl)(setf up ( * -1 len))(setf right 0)))
  (list (+ (car peg_pos) up)
        (+ (cadr peg_pos) right))))

; Function is_filled_peg_pos
; Returns T if a peg position is filled.  Nil if it is invalid
; or empty
(defun is_filled_peg_pos (board peg_pos)
  (if (is_valid_peg_pos board peg_pos)
    (nth (cadr peg_pos) (nth (car peg_pos) board))
    nil))

; Function is_empty_peg_pos
; Returns T if a peg position is empty.  Nil if it is invalid
; or filled.
(defun is_empty_peg_pos (board peg_pos)
  (if (is_valid_peg_pos board peg_pos)
    (not (nth (cadr peg_pos) (nth (car peg_pos) board)))
    nil))

; Function is_valid_peg_pos
; Takes a board and peg_pos and returns
; T if the peg_pos is valid
(defun is_valid_peg_pos (board peg_pos)
  (let ((row (car peg_pos))(pos (cadr peg_pos)))

    (if (and (>= row 0) (>= pos 0))
      (and (<= row (- (length board) 1))
          (<= pos (- (length (nth row board)) 1)))
      nil)))

; Function set_peg_pos
; Sets a board position to a value
(defun set_peg_pos (board peg_pos value)
  (setf (nth (cadr peg_pos) (nth (car peg_pos) board))
        value))
