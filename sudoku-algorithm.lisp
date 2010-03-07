(defparameter *board-width* 9)
(defparameter *board* (make-array (list *board-width* *board-width*)))
(defparameter *row-unit* 0)
(defparameter *column-unit* 1)
(defparameter *square-unit* 2)

;;; This class represents units that contain an array of cells.
;;; A unit can include cells in a row of Sudoku board,
;;; cells in a column of board, or cells in a square of board.
(defclass unit ()
    ((%cells :initarg :cells :accessor cells)))

(defun make-unit ()
  (make-instance 'unit :cells (make-array *board-width*)))

;;; This class represents cells that contain possible values.
;;; These cells also contain an array of units to which they belong.
(defclass cell ()
    ((%parents :initarg :parents :accessor parents)
     (%possibilities :initarg :possibilities :accessor possibilities)))

(defun make-cell ()
  (make-instance 'cell
    :parents (make-array 3)
    :possibilities (make-array *board-width* :element-type 'bit :initial-element 1)))

(defun erase-possibilities (cell)
  (let* ((leng (length (possibilities cell)))
	 (tmp (make-array leng :element-type 'bit :initial-element 0)))
    (setf (possibilities cell) tmp)))

(defun set-possibilities (cell index value)
  (setf (aref (possibilities cell) index) value))

(defun check-hole (cell)
  (if (= (symbolset-cardinality (possibilities cell)) 1)
    (return-from check-hole nil))
  t)

(defun find-possibilities (cell)
  (let ((tmp (make-array *board-width* :element-type 'bit :initial-element 1)))
    (loop for unit across (parents cell)
	  do (loop for cell1 across (cells unit)
		   do (unless (check-hole cell1)
			(let ((order-1 (position 1 (possibilities cell1))))
			  (setf (aref tmp order-1) 0)))))
    (setf (possibilities cell) tmp)))

;;; This class represents a Sudoku board.
;;; It includes an array of symbols that is used to display characters,
;;; an array of cells in the board,
;;; and an array of all units (row-units, column-units, square-units).
(defclass game ()
    ((%symbols :initarg :symbols :accessor symbols)
     (%cells :initarg :cells :accessor cells)
     (%units :initarg :units :accessor units)))

(defun make-game ()
  (make-instance 'game
    :symbols (make-array *board-width*)
    :cells (make-array (list *board-width* *board-width*))
    :units (make-array (list 3 *board-width*))))

(defparameter *game* (make-game))
(defparameter *unit* (make-unit))

(defun create-mapping-vector (game)
  (let* ((board-width (array-dimension (cells game) 0))
	 (v (make-array board-width)))
    (loop for i from 0 below board-width
	  do (setf (aref v i) (1+ i)))
    ;;(shuffle-vector v) This statement is commented for easy checking
    (setf (symbols game) v)))

(defun map-over-rectangle (fun game start-row height start-col width)
  (loop for row from start-row
	repeat height
	do (loop for col from start-col
		 repeat width
		 do (funcall fun (aref (cells game) row col)))))

(defun copy-rectangle-to-unit (game start-row height start-col width unit)
  (let ((unit-index -1))
    (map-over-rectangle (lambda (cell) (setf (aref (cells unit) (incf unit-index)) cell))
			game start-row height start-col width)))

(defun set-parents-of-cell (unit type)
  (loop for cell across (cells unit)
	do (setf (aref (parents cell) type) unit)))

(defun symbolset-cardinality (symbolset)
  (count 1 symbolset))

(defun symbolset-union (symbolset1 symbolset2)
  (bit-ior symbolset1 symbolset2))

(defun symbolset-difference (symbolset1 symbolset2)
  (bit-andc2 symbolset1 symbolset2))

(defun create-sample-unit (unit choice)
  (let ((unit1 (make-unit))
	(unit2 (make-unit))
	(unit3 (make-unit)))
    (loop for i from 0 below *board-width*
	  do (let ((cell (make-cell)))
	       (erase-possibilities cell)
	       (set-possibilities cell i 1)
	       (setf (aref (cells unit1) i) cell)))
    (setf (possibilities (aref (cells unit1) 3)) #*000110000)
    (loop for i from 0 below *board-width*
	  do (let ((cell (make-cell)))
	       (setf (possibilities cell) #*111111000)
	       (setf (aref (cells unit2) i) cell)))
    (setf (possibilities (aref (cells unit2) 3)) #*000001100)
    (setf (possibilities (aref (cells unit2) 5)) #*000001010)
    (loop for i from 0 below *board-width*
	  do (let ((cell (make-cell)))
	       (setf (possibilities cell) #*111111000)
	       (setf (aref (cells unit3) i) cell)))
    (setf (possibilities (aref (cells unit3) 3)) #*001000000)
    (setf (possibilities (aref (cells unit3) 5)) #*000010000)
    (cond ((= choice 1) (setf (cells unit) (cells unit1)))
      ((= choice 2) (setf (cells unit) (cells unit2)))
      ((= choice 3) (setf (cells unit) (cells unit3))))))

(defun strategy1 (unit)
  (let ((result '())
	(ambiguous-cells
	 (remove-if-not
	  (lambda (cell) (equalp (check-hole cell) t))
	  (cells unit))))
    (if (= (length ambiguous-cells) 1)
      ;; then the strategy is applicable
      (let ((cell (aref ambiguous-cells 0)))
	(setf (possibilities cell)
	      (symbolset-difference (possibilities cell)
				    (reduce #'symbolset-union
					    (remove cell (cells unit))
					    :key #'possibilities)))
	(push cell result)))
    result))

(defun strategy2 (unit)
  (let ((result '())
	(ambiguous-cells
	 (remove-if-not
	  (lambda (cell) (equalp (check-hole cell) t))
	  (cells unit))))
    (loop for cell across ambiguous-cells
	  do (let ((diff (symbolset-difference (possibilities cell)
					       (reduce #'symbolset-union
						       (remove cell (cells unit))
						       :key #'possibilities))))
	       (when (= (symbolset-cardinality diff) 1)
		 (setf (possibilities cell) diff)
		 (push cell result))))
    result))

(defun strategy3 (unit)
  (let ((result '())
	(ambiguous-cells
	 (remove-if-not
	  (lambda (cell) (equalp (check-hole cell) t))
	  (cells unit)))
	(unhole-cells
	 (remove-if
	  (lambda (cell) (equalp (check-hole cell) t))
	  (cells unit))))
    (loop for cell across unhole-cells
	  do (let ((order-1 (position 1 (possibilities cell))))
	       (loop for hole across ambiguous-cells
		     do (when (= (aref (possibilities hole) order-1) 1)
			  (set-possibilities hole order-1 0)
			  (push hole result)))))
    result))

(defun create-initial-board (board)
  (let* ((board-width (array-dimension board 0))
	 (sqrt-board-width (isqrt board-width)))
    (loop for offset1 from 0 below sqrt-board-width
	  for block-start from 0 below board-width by sqrt-board-width
	  do (loop for row from block-start
		   for offset2 from 0 by sqrt-board-width
		   repeat sqrt-board-width
		   do (loop for col from 0 below board-width
			    do (setf (aref board row col)
				     (mod (+ col offset1 offset2) board-width)))))))

;;; Exchange two non-overlapping rectangles having the same shape
(defun exchange-rectangles (board start-row-1 start-col-1 height width start-row-2 start-col-2)
  (unless (and (= start-row-1 start-row-2) (= start-col-1 start-col-2))
    (loop for row-1 from start-row-1
	  for row-2 from start-row-2
	  repeat height
	  do (loop for col-1 from start-col-1
		   for col-2 from start-col-2
		   repeat width
		   do (rotatef (aref board row-1 col-1)
			       (aref board row-2 col-2))))))

(defun exchange-rows (board row-1 row-2)
  (let ((board-width (array-dimension board 0)))
    (exchange-rectangles board row-1 0 1 board-width row-2 0)))

(defun exchange-columns (board col-1 col-2)
  (let ((board-height (array-dimension board 1)))
    (exchange-rectangles board 0 col-1 board-height 1 0 col-2)))

(defun exchange-blocks-of-rows (board row-1 row-2)
  (let* ((board-width (array-dimension board 0))
	 (sqrt-board-width (isqrt board-width)))
    (exchange-rectangles board row-1 0 sqrt-board-width board-width row-2 0)))

(defun exchange-blocks-of-columns (board col-1 col-2)
  (let* ((board-width (array-dimension board 0))
	 (sqrt-board-width (isqrt board-width)))
    (exchange-rectangles board 0 col-1 board-width sqrt-board-width 0 col-2)))

(defun compute-random-permutation (n exchange-fun)
  (loop for i from 0 to (- n 2)
	do (let ((e (+ i (random (- n i)))))
	     (unless (= i e)
	       (funcall exchange-fun i e)))))

(defun shuffle-vector (vector)
  (let ((N (length vector)))
    (compute-random-permutation
     N (lambda (i j) (rotatef (aref vector i) (aref vector j))))))

(defun shuffle-rows-within-block (board block-start)
  (let ((N (isqrt (array-dimension board 0))))
    (compute-random-permutation
     N (lambda (r1 r2) (exchange-rows board (+ block-start r1) (+ block-start r2))))))

(defun shuffle-columns-within-block (board block-start)
  (let ((N (isqrt (array-dimension board 0))))
    (compute-random-permutation
     N (lambda (c1 c2) (exchange-columns board (+ block-start c1) (+ block-start c2))))))

(defun shuffle-blocks-of-rows (board)
  (let ((N (isqrt (array-dimension board 0))))
    (compute-random-permutation
     N (lambda (r1 r2) (exchange-blocks-of-rows board (* r1 N) (* r2 N))))))

(defun shuffle-blocks-of-columns (board)
  (let ((N (isqrt (array-dimension board 0))))
    (compute-random-permutation
     N (lambda (c1 c2) (exchange-blocks-of-columns board (* c1 N) (* c2 N))))))

(defun shuffle-board (board)
  (let* ((board-width (array-dimension board 0))
	 (sqrt-board-width (isqrt board-width)))
    (loop for block-start from 0 below board-width by sqrt-board-width
       do (shuffle-rows-within-block board block-start))
    (loop for block-start from 0 below board-width by sqrt-board-width
       do (shuffle-columns-within-block board block-start))
    (shuffle-blocks-of-rows board)
    (shuffle-blocks-of-columns board)))

(defun set-sample-game (game)
  (let ((easy-board
	 (make-array (list *board-width* *board-width*)
		     :initial-contents '((0 6 5 0 2 0 0 7 0)
					 (7 2 9 5 3 0 0 0 0)
					 (0 4 0 8 0 0 2 0 9)
					 (0 5 1 0 8 0 7 0 2)
					 (0 0 0 7 0 2 0 0 0)
					 (2 0 7 0 4 0 3 9 0)
					 (6 0 4 0 0 1 0 2 0)
					 (0 0 0 0 9 8 6 3 7)
					 (0 9 0 0 6 0 1 4 0))))
	(hard-board
	 (make-array (list *board-width* *board-width*)
		     :initial-contents '((0 0 0 3 0 8 0 5 4)
					 (8 5 0 6 9 0 0 0 0)
					 (2 0 0 0 4 0 0 8 0)
					 (6 0 0 0 0 0 7 0 1)
					 (7 2 0 0 0 0 0 4 5)
					 (3 0 5 0 0 0 0 0 2)
					 (0 7 0 0 5 0 0 0 3)
					 (0 0 0 0 8 6 0 2 9)
					 (4 6 0 9 0 3 0 0 0))))
	(used-board (make-array (list *board-width* *board-width*))))
    (setf used-board easy-board)
    (loop for row from 0 below *board-width*
	  do (loop for col from 0 below *board-width*
		   for value = (aref used-board row col)
		   do (when (> value 0)
			(erase-possibilities (aref (cells game) row col))
			(set-possibilities (aref (cells game) row col) (- value 1) 1))))))

(defun create-game (board)
  (let* ((board-width (array-dimension board 0))
	 (sqrt-board-width (isqrt board-width))
	 (game (make-game)))
    ;; Create an array of cell instances
    (loop for row from 0 below board-width
	  do (loop for col from 0 below board-width
		   do (setf (aref (cells game) row col) (make-cell))))
    (set-sample-game game)
    ;; Create unit instances corresponding to rows
    (loop for row from 0 below board-width
	  for unit = (aref (units game) *row-unit* row)
	  do (setf unit (make-unit))
	     (copy-rectangle-to-unit game row 1 0 board-width unit)
	     (set-parents-of-cell unit *row-unit*))
    ;; Create unit instances corresponding to columns
    (loop for col from 0 below board-width
	  for unit = (aref (units game) *column-unit* col)
	  do (setf unit (make-unit))
	     (copy-rectangle-to-unit game 0 board-width col 1 unit)
	     (set-parents-of-cell unit *column-unit*))
    ;; Create unit instances corresponding to small squares
    (let ((i -1))
      (loop for row from 0 below board-width by sqrt-board-width
	    do (loop for col from 0 below board-width by sqrt-board-width
		     for unit = (aref (units game) *square-unit* (incf i))
		     do (setf unit (make-unit))
			(copy-rectangle-to-unit game row sqrt-board-width col sqrt-board-width unit)
			(set-parents-of-cell unit *square-unit*))))
    (setf *game* game)))

(defun solve-partial-board (game)
  (let ((board-width (array-dimension (cells game) 0)))
    (loop for row from 0 below board-width
	  do (loop for col from 0 below board-width
		   for cell = (aref (cells game) row col)
		   do (when (check-hole cell)
			(find-possibilities cell))))
    (format t "~%First game")
    (print-board game)
    (loop for i from 1 to 2
	  do (format t "~%Step ~S" i)
	     (print-board game))
    (strategy1 (aref (units game) *square-unit* 8))))

(defun print-board (game)
  (let ((board-width (array-dimension (cells game) 0)))
    (create-mapping-vector game)
    (loop for row from 0 below board-width
	  do (format t "~%")
	     (loop for col from 0 below board-width
		   for cell = (aref (cells game) row col)
		   do (if (check-hole cell)
			(format t "- ")
			(let ((order-1 (position 1 (possibilities cell))))
			  (format t "~S " (aref (symbols game) order-1))))))))
  
(defun main ()
  (create-initial-board *board*)
  (create-game *board*)
  (solve-partial-board *game*))

(defun test-strategy ()
  (create-sample-unit *unit* 3)
  (loop for i from 0 below *board-width*
	do (describe (aref (cells *unit*) i)))
  (strategy3 *unit*))