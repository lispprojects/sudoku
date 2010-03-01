(in-package :clim-user)

;;; Using presentations and presentation types to draw sudoku board.

;;; This class represents cells that display values.
;;; These cells can not be modified.
(defclass fixed-cell ()
  ((%label :initarg :name :reader label)
   (%duplication :initarg :name :accessor duplication)))

(defun make-fixed-cell (label)
  (make-instance 'fixed-cell :name label))

;;; This class represents cells those right values are hidden.
;;; They can be modified by users.
(defclass hole ()
  ((%label :initarg :name :accessor label)
   (%duplication :initarg :name :accessor duplication)))

(defun make-hole (label)
  (make-instance 'hole :name label))

;;; This class represents cells that display the inputting-values.
;;; They can not be modified by users.
(defclass sudoku-symbol ()
  ((%label :initarg :name :reader label)))

(defun make-sudoku-symbol (label)
  (make-instance 'sudoku-symbol :name label))

(defparameter *board-size* 9)
(defparameter *cell-width* 35)
(defparameter *thin-line-width* 2)
(defparameter *thick-line-width* 4)
(defparameter *initial-x* 30)
(defparameter *initial-y* 50)
(defparameter *hole-to-be-changed* NIL)
(defparameter *temp-array-to-display*  
  (make-array (list 9 9)
	      :initial-contents '((3 nil nil 1 nil nil 8 2 nil)
				  (nil nil nil nil nil nil 6 nil 9)
				  (nil 7 nil nil 6 nil nil nil nil)
				  (nil 3 nil nil 1 8 nil nil nil)
				  (nil 1 nil nil nil 9 nil nil 7)
				  (nil nil nil nil nil nil nil 9 8)
				  (6 nil nil nil 5 nil nil nil nil)
				  (8 2 nil nil nil nil 7 nil 4)
				  (nil nil 1 nil nil nil nil nil nil))))
(defparameter *labels-list* NIL)

(defun make-gui-array (board-size)
  (let ((array (make-array (list board-size board-size))))
    (dotimes (r board-size)
      (dotimes (c board-size)
        (setf (aref array r c)
              (if (= r c)
                  (make-hole (format nil "~a~a" r c))
                  (make-fixed-cell (format nil "~a~a" r c))))))
    array))

(defun make-gui-array-2 (array-to-display)
  (let ((array (make-array (list 9 9 ))))
    (loop for r from 0 below 9
	  do (loop for c from 0 below 9
		   do (setf (aref array r c)
			    (if (equal (aref array-to-display r c) 
				       nil)
				(make-hole (format nil ""))
				(make-fixed-cell 
				 (format nil "~a" 
					 (aref array-to-display r c)))))))
    array))

(defun make-sudoku-symbol-array (board-size)
  (let ((array (make-array (list board-size))))
    (loop for i from 0 below board-size
	  do (setf (aref array i)
		   (make-sudoku-symbol (format nil "~a" (+ i 1)))))
    array))

;;(defparameter *gui-array* (make-gui-array *board-size*))
(defparameter *gui-array* (make-gui-array-2 *temp-array-to-display*))
(defparameter *sudoku-symbol* (make-sudoku-symbol-array *board-size*))

(defun search-pos (board-size gui-array item)
  (let ((array-pos))  
    (loop for r from 0 below board-size
	  do (loop for c from 0 below board-size
		   do (if (equal item (aref gui-array r c))
			  (setf array-pos (list r c)))))
    array-pos))

(defun search-items-duplication (board-size gui-array label)
  (let ((labels-list))
    (loop for r from 0 below board-size
	  do (loop for c from 0 below board-size
		   do (if (equal label (label (aref gui-array r c)))
			  (setf labels-list (append labels-list 
						    (list label r c))))))
    labels-list))

(defun check-in-same-block (r1 c1 r2 c2 board-size)
  (let ((checking nil))
    (loop for r-start-point from 0 
	    below board-size by (ceiling (sqrt board-size))
	  do (loop for c-start-point from 0 
		     below board-size by (ceiling (sqrt board-size))
		   do (let ((count 0))
			(loop for r from r-start-point 
				below (+ r-start-point 
					 (ceiling (sqrt board-size)))
			      do (loop for c from c-start-point 
					 below (+ c-start-point 
						  (ceiling (sqrt board-size)))
				       do (if (or (and (equal r r1)
						       (equal c c1))
						  (and (equal r r2)
						       (equal c c2)))
					      (incf count))))
			(if (equal 2 count)
			    (setf checking t)))))
    checking))

(defun lets-search (labels-list gui-array)
  (let ((list-length (length labels-list)))
    (flet ((assign-dup-value (r-pos c-pos bool-value)
	     (setf (duplication (aref gui-array
				      r-pos
				      c-pos))
		   bool-value)))
      (if (> list-length 3)
	  (progn (loop for i from 1 below list-length by 3
		       do (assign-dup-value (nth i labels-list)
					    (nth (1+ i) labels-list)
				 nil))
		 (loop for i from 1 below list-length by 3
		       do (loop for j from (+ i 3) below list-length by 3
				do (if (or (equal (nth j labels-list) 
						  (nth i labels-list))
					   (equal (nth (1+ j) labels-list) 
						  (nth (1+ i) labels-list))
					   (equal (check-in-same-block 
						   (nth j labels-list)
						   (nth (1+ j) labels-list)
						   (nth i labels-list) 
						   (nth (1+ i) labels-list) 
						   *board-size*) 
						  t))
				       (progn (assign-dup-value 
					       (nth j labels-list)
					       (nth (1+ j) labels-list)
					       t)
					      (assign-dup-value 
					       (nth i labels-list)
					       (nth (1+ i) labels-list)
					       t))))))
	  (if (> list-length 0)
	      (assign-dup-value (nth 1 labels-list)
				(nth 2 labels-list)
				nil))))))

(define-application-frame sudoku-gui ()
  ((%cells :initform *gui-array*
	   :accessor cells)
   (%symbols :initform *sudoku-symbol*
	     :accessor symbols))
  (:menu-bar menubar-command-table)
  (:panes (app :application-pane
	       :height 760  :width 760
	       :display-function 'display-app)
	  (int :interactor
	       :height 20 :width 760))
  (:layouts (default (vertically () app int))))

(defun draw-lines (pane 
		   board-size cell-width 
		   thin-line-width thick-line-width 
		   initial-x initial-y)
  (flet ((plus-plus-minus (initial line-width time)
	   (+ (+ (- initial line-width)
		 (* (+ cell-width thin-line-width) time))
	      (* (floor (/ time (sqrt board-size)))
		 (- thick-line-width thin-line-width))))
	 (plus-plus-board-size (initial)
	   (+ (+ initial (* (+ cell-width thin-line-width) board-size))
	      (* (/ board-size (sqrt board-size))
		 (- thick-line-width thin-line-width))))
	 (plus-plus-time (initial time)
	   (+ (+ initial (* (+ cell-width thin-line-width) time))
	      (* (floor (/ time (sqrt board-size)))
		 (- thick-line-width thin-line-width)))))
    (flet ((draw-horizontal-line (type-of-line time)
	     (clim:draw-rectangle* pane
				   (- initial-x thick-line-width)
				   (plus-plus-minus initial-y type-of-line time)
				   (plus-plus-board-size initial-x)
				   (plus-plus-time initial-y time)))
	   (draw-vertical-line (type-of-line time)
	     (clim:draw-rectangle* pane
				   (plus-plus-minus initial-x type-of-line time)
				   initial-y
				   (plus-plus-time initial-x time)
				   (plus-plus-board-size initial-y))))
      (loop for time from 0 to board-size
	    do (if (zerop (mod time (sqrt board-size)))
		   (progn (draw-horizontal-line thick-line-width time)
			  (draw-vertical-line thick-line-width time))
		   (progn (draw-horizontal-line thin-line-width time)
			  (draw-vertical-line thin-line-width time)))))))

(defun display-app (frame pane)
  (declare (ignore frame))
  (let ((x *initial-x*) (y *initial-y*) (count-x 0) (count-y 0))
    (loop for row from 0 below *board-size*
	  do (loop for column from 0 below *board-size*
		   do (let ((obj (aref *gui-array* row column)))
			(with-output-as-presentation (pane obj (class-of obj))
			  (draw-rectangle* pane x y
					   (+ x *cell-width*) (+ y *cell-width*)
					   :ink (if (typep obj 'fixed-cell)
						    +green+
						    +orange+))))
		   do (let ((obj (aref *gui-array* row column)))
			(draw-text* pane (label (aref *gui-array* row column))
				    (+ x (/ *cell-width* 2)) (+ y (/ *cell-width* 2))
				    :align-x :center :align-y :center
				    :ink (if (equal (duplication obj) t)
					     +red+
					     +black+)))
		   do (incf count-x)
		   do (if (= count-x (sqrt *board-size*))
			  (progn (incf x (+ *cell-width* *thick-line-width*))
				 (setf count-x 0))
			  (incf x (+ *cell-width* *thin-line-width*))))
	  do (incf count-y)
	  do (setf x *initial-x*)
	  do (if (= count-y (sqrt *board-size*))
		 (progn (incf y (+ *cell-width* *thick-line-width*))
			(setf count-y 0))
		 (incf y (+ *cell-width* *thin-line-width*)))))
  (draw-lines pane 
	      *board-size* *cell-width* 
	      *thin-line-width* *thick-line-width* 
	      *initial-x* *initial-y*)
  (let ((symbol-x (+ *initial-x* (/ *cell-width* 2))))
    (loop for i from 0 below *board-size*
	  do (let ((obj (aref *sudoku-symbol* i)))
	       (with-output-as-presentation (pane obj (class-of obj))
		 (draw-text* pane (label (aref *sudoku-symbol* i))
			     symbol-x 30)))
	  do (incf symbol-x *cell-width*))))

(defun sudoku-gui ()
  (run-frame-top-level (make-application-frame 'sudoku-gui)))

(make-command-table 'file-command-table
		    :errorp nil
		    :menu '(("New game" :command new-game)
			    ("Save game" :command save-game)
			    ("Load game" :command load-game)))

(make-command-table 'difficulties-command-table
		    :errorp nil
		    :menu '(("Easy" :command easy-mode)
			    ("Medium" :command medium-mode)
			    ("Hard" :command hard-mode)))
(make-command-table 'board-size-command-table
		    :errorp nil
		    :menu '(("4x4" :command size-four)
			    ("9x9 (Default)" :command size-nine)
			    ("16x16" :command size-sixteen)))
(make-command-table 'cell-size-command-table
		    :errorp nil
		    :menu '(("Standard (Default)" :command cell-standard)
			    ("Large" :command cell-large)))
(make-command-table 'lines-size-command-table
		    :errorp nil
		    :menu '(("Standard (Default)" :command lines-standard)
			    ("Bolder" :command lines-bolder)))

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("File" :menu file-command-table)
			    ("Difficulties" :menu difficulties-command-table)
			    ("Board-sizes" :menu board-size-command-table)
			    ("Cell-sizes" :menu cell-size-command-table)
			    ("Line-sizes" :menu lines-size-command-table)
			    ("Quit" :command quit)))

(defun combo-command-for-size (size)
  (setf *board-size* size)
  (setf *gui-array* (make-gui-array *board-size*))
  (setf *sudoku-symbol* (make-sudoku-symbol-array *board-size*))
  (setf *hole-to-be-changed* NIL))

(define-sudoku-gui-command new-game ())

(define-sudoku-gui-command save-game ())

(define-sudoku-gui-command load-game ())

(define-sudoku-gui-command easy-mode ())

(define-sudoku-gui-command medium-mode ())

(define-sudoku-gui-command hard-mode ())

(define-sudoku-gui-command size-four ()
  (combo-command-for-size 4))

(define-sudoku-gui-command size-nine ()
  (combo-command-for-size 9))

(define-sudoku-gui-command size-sixteen ()
  (combo-command-for-size 16))

(define-sudoku-gui-command cell-standard ()
  (setf *cell-width* 35))

(define-sudoku-gui-command cell-large ()
  (setf *cell-width* 42))

(define-sudoku-gui-command lines-standard ()
  (setf *thin-line-width* 2)
  (setf *thick-line-width* 4))

(define-sudoku-gui-command lines-bolder ()
  (setf *thin-line-width* 3)
  (setf *thick-line-width* 5))

(define-sudoku-gui-command quit ()
  (clim:frame-exit clim:*application-frame*))

(define-sudoku-gui-command (com-sudoku-symbol :name t)
    ((sudoku-symbol 'sudoku-symbol))
  (if (equal NIL *hole-to-be-changed*)
      (format (find-pane-named *application-frame* 'int)
	      "Please select a hole to input first")
      (progn (setf (label (aref *gui-array* (first *hole-to-be-changed*) 
				(second *hole-to-be-changed*))) 
		   (label sudoku-symbol))
	     (setf *hole-to-be-changed* NIL)))
  (loop for k from 1 to *board-size*
	do (setf *labels-list* (search-items-duplication 
				      *board-size* 
				      *gui-array* 
				      (format nil "~a" k)))
	do (lets-search *labels-list* *gui-array*)))

(define-presentation-to-command-translator com-sudoku-symbol-translator
    (sudoku-symbol com-sudoku-symbol sudoku-gui)
  (object)
  (list object))

(define-sudoku-gui-command (com-sudoku-hole :name t)
    ((hole 'hole))
  (let ((hole-position (search-pos *board-size* *gui-array* hole)))
    (setf *hole-to-be-changed* hole-position)))

(define-presentation-to-command-translator com-sudoku-hole-translator
    (hole com-sudoku-hole sudoku-gui)
  (object)
  (list object))

(define-sudoku-gui-command (com-sudoku-hole-error :name t)
    ((hole-error 'hole-error))
  (let ((hole-position (search-pos *board-size* *gui-array* hole-error)))
    (setf *hole-to-be-changed* hole-position)))

(define-presentation-to-command-translator com-sudoku-hole-error-translator
    (hole-error com-sudoku-hole sudoku-gui)
  (object)
  (list object))
