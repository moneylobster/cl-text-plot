;;;; implements a unicode canvas and plotting
;; TODOs
;; legend

(defpackage :textplot
			(:use :common-lisp)
			(:export
			 #:plot
			 #:plot-fun
			 #:plot-multiple
			 #:scatter
			 #:scatter-multiple
			 #:*colors-enabled*))

(in-package :textplot)

(defvar *colors-enabled* nil
  "If you want your plots to be printed with color using the ansi color
codes, set this to t. Your terminal must support ansi color codes.")

;;; Utility functions
(defun 2d+ (v1 v2)
  "add vectors v1 and v2 elementwise, which are 2-element lists"
  (list (+ (first v1) (first v2))
		(+ (second v1) (second v2))))

(defun 2dneg (v1)
  "invert the sign of v1, which is a 2-element list.
eg. (2dneg v1) => -v1"
  (list (- (first v1))
		(- (second v1))))

(defun 2ddot (v1 v2)
  "dot product of v1 and v2"
  (+ (* (first v1) (first v2))
	 (* (second v1) (second v2))))

(defun best-worst (fn lst)
  "Get the best and worst of list according to function.
Adapted from pg - on lisp

Examples:
(best-worst #'> '(1 2 3 4 5 6 7 8 -1 44 23))
=> 44, -1"
  (if (null lst)
	  nil
	  (let ((wins (car lst))
			(loses (car lst)))
		(dolist (obj (cdr lst))
		  (when (funcall fn obj wins)
			  (setq wins obj))
		  (when (funcall fn loses obj)
			  (setq loses obj)))
		(list wins loses))))

(defun collision-check-rect (pt rect)
  "Check if point pt is inside the rectangle given by the 4 corners in rect.

Examples:
(collision-check-rect '(3 4) '((0 0) (0 5) (5 5) (5 0)))
=> t
(collision-check-rect '(6 6) '((0 0) (0 5) (5 5) (5 0)))
=> nil"
  (let* ((ap (list (- (first pt) (first (first rect)))
				   (- (second pt) (second (first rect)))))
		 (ab (list (- (first (second rect)) (first (first rect)))
				   (- (second (second rect)) (second (first rect)))))
		 (ad (list (- (first (third rect)) (first (first rect)))
				   (- (second (third rect)) (second (first rect))))))
										; check if vector to point
										; from one corner is within
										; two edges
	(and (<= 0 (2ddot ap ab) (2ddot ab ab))
		 (<= 0 (2ddot ap ad) (2ddot ad ad)))))

(defun collision-check-circle (pt center radius)
  "Check if point pt is inside circle given by center and radius.

Examples:
(collision-check-circle '(0.5 0.5) '(1 1) 3)
=> t
(collision-check-circle '(10 10) '(1 1) 3)
=> nil"
  (let* ((cp (list (- (first pt) (first center))
				   (- (second pt) (second center)))))
	(<= (sqrt (2ddot cp cp)) radius)))

(defun array-map (fn array)
  "map a function to an array"
  (let ((res (make-array (array-dimensions array))))
	(dotimes (i (array-total-size array) res)
	  (setf (row-major-aref res i)
			(funcall fn (row-major-aref array i))))))

(defun justify-center (textlen spacelen)
  "Place text of length textlen into a space of length spacelen.
Returns index for where the text should start from in the space.

Examples:
for ___TEXT___: textlen=4, spacelen=10
(justify-center 4 10)
=> 3
"
  (if (<= textlen spacelen)
	  (truncate (/ (- spacelen textlen) 2))
	  0))

(defun wrap-when (test fn arg)
  "Call function FN with ARG if test is true, otherwise return ARG.

Examples:
(wrap-when t #'sqrt 4)
=> 2
(wrap-when nil #'sqrt 4)
=> 4"
  (if test (funcall fn arg) arg))

(defun flatten-1 (lst)
  "Flatten list LST one level.

Examples
(flatten-1 '((1 2) 3 4 ((5 6) 7)))
=> '(1 2 3 4 (5 6) 7)"
  (let ((acc nil))
	(dolist (el lst)
	  (if (listp el)
		  (dolist (el1 el)
			(push el1 acc))
		  (push el acc)))
	(nreverse acc)))

;; 4-bit color codes
(defconstant +black+ 30)
(defconstant +red+ 31)
(defconstant +green+ 32)
(defconstant +yellow+ 33)
(defconstant +blue+ 34)
(defconstant +magenta+ 35)
(defconstant +cyan+ 36)
(defconstant +white+ 37)
(defconstant +bright-black+ 90)
(defconstant +bright-red+ 91)
(defconstant +bright-green+ 92)
(defconstant +bright-yellow+ 93)
(defconstant +bright-blue+ 94)
(defconstant +bright-magenta+ 95)
(defconstant +bright-cyan+ 96)
(defconstant +bright-white+ 97)
(defconstant +4bit-colormap+ (list +red+ +green+ +yellow+
								   +blue+ +magenta+ +cyan+
								   +bright-red+ +bright-green+
								   +bright-yellow+ +bright-blue+
								   +bright-magenta+ +bright-cyan+))

(defun colorize (string color)
  "Wrap a string with color codes.

Examples:
(colorize \"hello\" 31)
=>hello in red"
  (format nil "~A[~Am~A~A[0m" #\Esc color string #\Esc))

;; canvas backends
(defclass canvas ()
  ((primitives
	:initarg :primitives
	:accessor primitives
	:documentation "List of characters to be used for the cells. The first element should be the empty one.")
   (cell-resolution
	:initarg :cell-resolution
	:accessor cell-resolution
	:documentation "Two-element list denoting the pixel resolution of each cell as (x y)")
   (canvas-size
	:initarg :canvas-size
	:accessor canvas-size
	:documentation "Two-element list denoting the canvas size in characters as (x y)")
   (canvas
	:initarg :canvas
	:accessor canvas
	:documentation "x*y character array containing the plot itself. Doesn't include labels/titles etc.")
   (canvas-colors
	:initarg :canvas-colors
	:accessor canvas-colors
	:documentation "x*y number array containing the foreground color code of the corresponding characters in the plot.")))

(defmethod initialize-instance :after ((c canvas) &rest args)
  "Create a canvas as an array of size x y.
Actual resolution is x*CELLRESX x y*CELLRESY"
  (declare (ignorable args))
  (setf (canvas c)
		(make-array (reverse (canvas-size c)) :initial-element (first (primitives c))))
  (setf (canvas-colors c)
		(make-array (reverse (canvas-size c)) :initial-element 0)))

(defclass braille-canvas (canvas)
  ()
  (:default-initargs
   :primitives '(#\BRAILLE_PATTERN_BLANK
				 #\BRAILLE_PATTERN_DOTS-1
				 #\BRAILLE_PATTERN_DOTS-2
				 #\BRAILLE_PATTERN_DOTS-3
				 #\BRAILLE_PATTERN_DOTS-4
				 #\BRAILLE_PATTERN_DOTS-5
				 #\BRAILLE_PATTERN_DOTS-6
				 #\BRAILLE_PATTERN_DOTS-7
				 #\BRAILLE_PATTERN_DOTS-8)
   :cell-resolution '(2 4))
  (:documentation "A canvas using braille characters.
For the primitives, 0 is empty, 1-8 are as follows:
1 4
2 5
3 6
7 8"))

(defclass blocks-canvas (canvas)
  ()
  (:default-initargs
   :primitives '(#\SPACE
				 #\BLOCK_SEXTANT-1
				 #\BLOCK_SEXTANT-2
				 #\BLOCK_SEXTANT-12
				 #\BLOCK_SEXTANT-3
				 #\BLOCK_SEXTANT-13
				 #\BLOCK_SEXTANT-23
				 #\BLOCK_SEXTANT-123
				 #\BLOCK_SEXTANT-4
				 #\BLOCK_SEXTANT-14
				 #\BLOCK_SEXTANT-24
				 #\BLOCK_SEXTANT-124
				 #\BLOCK_SEXTANT-34
				 #\BLOCK_SEXTANT-134
				 #\BLOCK_SEXTANT-234
				 #\BLOCK_SEXTANT-1234
				 #\BLOCK_SEXTANT-5
				 #\BLOCK_SEXTANT-15
				 #\BLOCK_SEXTANT-25
				 #\BLOCK_SEXTANT-125
				 #\BLOCK_SEXTANT-35
				 #\LEFT_HALF_BLOCK
				 #\BLOCK_SEXTANT-235
				 #\BLOCK_SEXTANT-1235
				 #\BLOCK_SEXTANT-45
				 #\BLOCK_SEXTANT-145
				 #\BLOCK_SEXTANT-245
				 #\BLOCK_SEXTANT-1245
				 #\BLOCK_SEXTANT-345
				 #\BLOCK_SEXTANT-1345
				 #\BLOCK_SEXTANT-2345
				 #\BLOCK_SEXTANT-12345
				 #\BLOCK_SEXTANT-6
				 #\BLOCK_SEXTANT-16
				 #\BLOCK_SEXTANT-26
				 #\BLOCK_SEXTANT-126
				 #\BLOCK_SEXTANT-36
				 #\BLOCK_SEXTANT-136
				 #\BLOCK_SEXTANT-236
				 #\BLOCK_SEXTANT-1236
				 #\BLOCK_SEXTANT-46
				 #\BLOCK_SEXTANT-146
				 #\RIGHT_HALF_BLOCK
				 #\BLOCK_SEXTANT-1246
				 #\BLOCK_SEXTANT-346
				 #\BLOCK_SEXTANT-1346
				 #\BLOCK_SEXTANT-2346
				 #\BLOCK_SEXTANT-12346
				 #\BLOCK_SEXTANT-56
				 #\BLOCK_SEXTANT-156
				 #\BLOCK_SEXTANT-256
				 #\BLOCK_SEXTANT-1256
				 #\BLOCK_SEXTANT-356
				 #\BLOCK_SEXTANT-1356
				 #\BLOCK_SEXTANT-2356
				 #\BLOCK_SEXTANT-12356
				 #\BLOCK_SEXTANT-456
				 #\BLOCK_SEXTANT-1456
				 #\BLOCK_SEXTANT-2456
				 #\BLOCK_SEXTANT-12456
				 #\BLOCK_SEXTANT-3456
				 #\BLOCK_SEXTANT-13456
				 #\BLOCK_SEXTANT-23456
				 #\FULL_BLOCK)
   :cell-resolution '(2 3))
  (:documentation "A canvas using sextant block characters.
1-6 are oriented as follows:
1 2
3 4
5 6"))

(defun create-canvas (x y &optional (backend :blocks))
  "Create a canvas of size x y using BACKEND.
BACKEND can be one of :braille or :blocks.
Actual resolution is [x y].*cell-resolution of the backend."
  (make-instance (ccase backend
				   (:braille 'braille-canvas)
				   (:blocks 'blocks-canvas))
				 :canvas-size (list x y)))

(defgeneric add-cells (canvas &rest args))

(defmethod add-cells ((canvas braille-canvas) &rest args)
    "add braille characters

Examples:
  (let ((c (create-canvas 1 1 :braille)))
	(format t \"~A~%\" (add-cells c
								  (nth 2 (primitives c))
								  (nth 6 (primitives c)))))
=> prints ⠢
"
  (code-char (apply #'logior (mapcar #'char-code args))))

(defmethod add-cells ((canvas blocks-canvas) &rest args)
  "add block characters

Examples:
(add-cells (create-canvas 1 1 :blocks) #\BLOCK_SEXTANT-1 #\BLOCK_SEXTANT-35)
=> #\LEFT_HALF_BLOCK"
  (nth (apply #'logior (mapcar (lambda (x) (position x (primitives canvas)))
							   args))
	   (primitives canvas)))

(defgeneric xy-to-cell-index (canvas x y)
  (:documentation "Turn x y coordinates given for the cell grid into the corresponding
primitives list index"))

(defmethod xy-to-cell-index ((canvas braille-canvas) x y)
  (nth (+ x (* 2 y)) '(1 4 2 5 3 6 7 8)))

(defmethod xy-to-cell-index ((canvas blocks-canvas) x y)
  (ash #b000001 (+ x (* 2 y))))

(defun turn-on! (canvas x y)
  "Turn on the pixel located at X,Y. Modifies CANVAS."
  (let ((canvas-data (canvas canvas))
		(cell-resolution (cell-resolution canvas)))
	(if (and (>= x 0)
			 (>= y 0)
			 (< x (* (first cell-resolution) (array-dimension canvas-data 1)))
			 (< y (* (second cell-resolution) (array-dimension canvas-data 0))))
		(multiple-value-bind (gridx blockx) (truncate x (first cell-resolution))
		  (multiple-value-bind (gridy blocky) (truncate y (second cell-resolution))
			(setf (aref canvas-data gridy gridx)
				  (add-cells canvas
							 (aref canvas-data gridy gridx)
							 (nth (xy-to-cell-index canvas
													(truncate blockx)
													(truncate blocky))
								  (primitives canvas))))))))
  canvas)

(defun set-color! (canvas x y color)
  "Set the canvas cell that contains the pixel X,Y to be of the color code COLOR. Modifies CANVAS."
  (let ((canvas-data (canvas-colors canvas))
		(cell-resolution (cell-resolution canvas)))
	(if (and (>= x 0)
			 (>= y 0)
			 (< x (* (first cell-resolution) (array-dimension canvas-data 1)))
			 (< y (* (second cell-resolution) (array-dimension canvas-data 0))))
		(multiple-value-bind (gridx blockx) (truncate x (first cell-resolution))
		  (multiple-value-bind (gridy blocky) (truncate y (second cell-resolution))
			(setf (aref canvas-data gridy gridx) color))))))
		  
							 

(defun print-canvas (canvas &key (fmt-stream t))
  "print the whole canvas as unicode characters.

Examples:
(print-canvas (create-canvas 6 6 :braille))"
  (let ((canvas-data (canvas canvas)))
	(dotimes (i (array-dimension canvas-data 0))
	  (dotimes (j (array-dimension canvas-data 1))
		(format fmt-stream "~A" (aref canvas-data i j)))
	  (format fmt-stream "~%"))))

;;; Canvas stuff - drawing
(defun draw-line! (canvas v1 v2 thickness &key color)
  "Draw a line from V1 to V2 with THICKNESS. Modifies CANVAS.
V1 and V2 are 2-element lists: (x,y)
COLOR is an optional color code to color the line with.

Examples:
(print-canvas (draw-line! (create-canvas 6 6 :braille) '(5 5) '(10 10) 1))
"
  (let* ((linevec (2d+ v2 (2dneg v1)))
		 (perpvec (list (- (second linevec)) (first linevec)))
		 (perpvec-scaled (mapcar (lambda (x)
								   (* (/ thickness 2)
									  (/ x (sqrt (2ddot perpvec perpvec)))))
								 perpvec))
		 (cornerpoints (list (2d+ v1 perpvec-scaled)
							 (2d+ v1 (2dneg perpvec-scaled))
							 (2d+ v2 perpvec-scaled)
							 (2d+ v2 (2dneg perpvec-scaled))))
		 (maxpoints (list (best-worst #'> (mapcar
										   (lambda (x) (truncate (first x)))
										   cornerpoints))
						  (best-worst #'> (mapcar
										   (lambda (x) (truncate (second x)))
										   cornerpoints)))))
	(mapcar (lambda (p)
			  (turn-on! canvas (first p) (second p))
			  (when color (set-color! canvas (first p) (second p) color)))
			(loop for x from (second (first maxpoints)) to (first (first maxpoints))
				  append
				  (loop for y from (second (second maxpoints)) to (first (second maxpoints))
						when (collision-check-rect (list x y) cornerpoints)
						  collect (list x y)))))
  canvas)

(defun draw-circle! (canvas center radius &key color)
  "Draw a circle with CENTER and RADIUS. Modifies CANVAS.
CENTER is a 2-element list (x y).
COLOR is an optional color code to color the line with.

Examples:
(print-canvas (draw-circle! (create-canvas 6 6 :braille) '(5 5) 5))
"
  (let* ((maxpoints (list (list (+ (first center) radius)
								(- (first center) radius))
						  (list (+ (second center) radius)
								(- (second center) radius)))))
	(mapcar (lambda (p)
			  (turn-on! canvas (first p) (second p))
			  (when color (set-color! canvas (first p) (second p) color)))
			(loop for x from (second (first maxpoints)) to (first (first maxpoints))
				  append
				  (loop for y from (second (second maxpoints)) to (first (second maxpoints))
						when (collision-check-circle (list x y) center radius)
						  collect (list x y)))))
  canvas)

(defun draw-frame! (canvas &key color)
  "Draw a frame to the edge of the canvas. Modifies CANVAS.
COLOR is an optional color code to color the line with.

Examples:
(print-canvas (draw-frame! (create-canvas 60 20 :braille)))
"
  (let* ((cell-resolution (cell-resolution canvas))
		 (canvas-data (canvas canvas))
		 (xlen (- (* (first cell-resolution) (array-dimension canvas-data 1)) 1))
		 (ylen (- (* (second cell-resolution) (array-dimension canvas-data 0)) 1)))
	(apply #'mapcar (lambda (x1 y1 x2 y2)
						  (draw-line! canvas (list x1 y1) (list x2 y2) 1 :color color))
		   (list (list 0 0 xlen 0)
				 (list 0 0 0 ylen)
				 (list xlen 0 xlen xlen)
				 (list 0 ylen ylen ylen))))
  canvas)

(defun draw-chart! (canvas points thickness &key color)
  "Draw lines connecting POINTS onto CANVAS with specified THICKNESS and
COLOR."
  (let ((last-pt nil))
	(dolist (pt points)
	  (if last-pt
			(draw-line! canvas last-pt pt thickness :color color))
		(setq last-pt pt))))

(defun draw-scatter! (canvas points thickness &key color)
  "Draw circles with THICKNESS on each of the POINTS on CANVAS with
COLOR."
  (dolist (pt points)
	(draw-circle! canvas pt thickness :color color)))

;;; Plotting stuff

(defun print-canvas-with-labels (canvas &key title xlabel ylabel xlim ylim (fmt-stream t))
  "Print the canvas with plot labels: a title, x and y axis labels and limits.
No legend for now.

Diagram:

+------------------------+
|      	 title     	   	 |
+----+-+----------+------+
|    |y|          |      |
|    | |          |      |
|ylab|l|  canvas  |legend|
|    |i|          |      |
|    |m|          |      |
+----+-+----------+------+
	   |  x	lim   |
   	   +----------+
	   |  xlabel  |
	   +----------+

Examples:
  (print-canvas-with-labels (draw-frame!
							 (draw-line!
							  (create-canvas 20 10 :braille)
							  '(10 10) '(20 30) 3))
							:title \"Hello\" :xlabel \"time\" :ylabel \"f(x)\"
							:xlim '(0 10) :ylim '(-10 10))
"

  (let* ((canvas-data (canvas canvas))
		 (canv-xlen (array-dimension canvas-data 1))
		 (canv-ylen (array-dimension canvas-data 0))
		 (titlelen (length title))
		 (xlablen (length xlabel))
		 (ylablen (length ylabel))
		 (ylimlen (if ylim
					  (apply #'max (mapcar (lambda (x) (length (write-to-string x))) ylim))
					  0))
		 (titlestart (justify-center titlelen (+ ylablen ylimlen canv-xlen)))
		 (xlabstart (+ ylablen ylimlen (justify-center xlablen canv-xlen)))
		 (titlecount (if title 1 0))
		 (xlimcount (if xlim 1 0))
		 (xlabelcount (if xlabel 1 0))
		 (linecount (+ 1 titlecount canv-ylen xlimcount xlabelcount)))
	(dotimes (lineno linecount fmt-stream)
	  (cond ((= (1+ lineno) titlecount)
			 (if title (format t "~v,@T~A~%" titlestart title))) ; title
			((< titlecount (1+ lineno) (+ titlecount canv-ylen 1))
			 (if (= lineno (truncate (/ canv-ylen 2))) ; ylabel
				 (if ylabel (format fmt-stream "~A" ylabel))
				 (format fmt-stream "~v,@T" ylablen))
			 (cond ((and ylim (= lineno titlecount))
					(format fmt-stream "~v@A" ylimlen (second ylim))) ; upper ylim
				   ((and ylim (= (1+ lineno) (+ titlecount canv-ylen)))
					(format fmt-stream "~v@A" ylimlen (first ylim))) ; lower ylim
				   (t
					(format fmt-stream "~v@T" ylimlen)))
			 (dotimes (xindex canv-xlen) ; canvas
			   (format fmt-stream "~A"
					   (wrap-when *colors-enabled*
								  (lambda (str) (colorize str
														  (aref (canvas-colors canvas)
																(- lineno titlecount) xindex)))
								  (aref canvas-data
										(- lineno titlecount) xindex))))
			 (format fmt-stream "~%"))
			((= lineno (+ 1 titlecount canv-ylen)) ; xlim
			 (if xlim (format fmt-stream "~v,@T~vA~v@A~%"
							  (+ ylablen ylimlen)
							  (truncate (/ canv-xlen 2))
							  (first xlim)
							  (truncate (/ canv-xlen 2))
							  (second xlim))))
			((= lineno (+ 2 titlecount canv-ylen)) ;xlabel
			 (if xlabel (format fmt-stream "~v,@T~A~%" xlabstart xlabel)))))))

(defun invert-y-axis (data ymax)
  "Subtract YMAX from the y component of each datapoint.
DATA is an Nx2 list.

Examples:
(invert-y-axis '((1 2) (3 4)) 4)
=> ((1 2) (3 0))
"
  (mapcar (lambda (x) (list (first x) (- ymax (second x)))) data))

(defun scale-data (data xmin xmax ymin ymax dataxminmax datayminmax)
  "Scale data so that the extrema in x and y correspond to xmax/min and ymax/min.

Examples:
(scale-data '((-1 0) (1 2)) 0 20 0 30 '(-1 1) '(0 2))
=> ((0 0) (20 30))
"
  (let* ((dataxmin (first dataxminmax))
		 (dataxmax (second dataxminmax))
		 (dataymin (first datayminmax))
		 (dataymax (second datayminmax))
		 (xratio (/ (- xmax xmin) (- dataxmax dataxmin)))
		 (yratio (/ (- ymax ymin) (- dataymax dataymin))))
	(mapcar (lambda (x)
			  (list (+ xmin (* xratio (- (first x) dataxmin)))
					(+ ymin (* yratio (- (second x) dataymin)))))
			data)))

(defun plot-scale-calculations (data zoom)
  "Repeated operations in plotting abstracted into a function.
Calculates the minimum and maximum values in x and y
and the ranges for the x and y axes.

Returns values: XMINMAX YMINMAX XLIM YLIM
each is a 2-element list."
  (let* ((xminmax (best-worst #'< (mapcar #'first data)))
		 (yminmax (best-worst #'< (mapcar #'second data)))
		 (xmidpoint (/ (+ (first xminmax) (second xminmax)) 2))
		 (ymidpoint (/ (+ (first yminmax) (second yminmax)) 2))
		 (xlim (mapcar (lambda (x)
						 (+ (/ (- x xmidpoint) (first zoom)) xmidpoint))
					   xminmax))
		 (ylim (mapcar (lambda (y)
						 (+ (/ (- y ymidpoint) (second zoom)) ymidpoint))
					   yminmax)))
	(values xminmax yminmax xlim ylim)))

(defun scale-points (data cell-resolution xminmax yminmax size zoom)
  "Scale and invert the data such that each datapoint corresponds to its
corresponding pixel on the canvas."
  (let ((xres (first cell-resolution))
		(yres (second cell-resolution)))
	(invert-y-axis
	 (scale-data data
				 (* xres (first size) (- 1 (first zoom)))
				 (* xres (first size) (first zoom))
				 (* yres (second size) (- 1 (second zoom)))
				 (* yres (second size) (second zoom))
				 xminmax yminmax)
	 (* yres (second size)))))

(defun plot (data &key title xlabel ylabel (size '(20 10)) (thickness 1) (zoom '(1 1)) (as-string nil) (backend :blocks))
  "Plot data as a line chart.

DATA: an Nx2 list of points.

TITLE: a string, printed above the plot.
XLABEL and YLABEL: strings describing the axes, printed next to the axes.
SIZE: plot size in characters as a list '(x y).
THICKNESS: line thickness.
ZOOM: how much to scale the plot as a list '(x y).
AS-STRING: if t, return plot as a string instead of printing.
BACKEND: which rendering backend to use.

Examples:
(plot '((1 2) (2 6) (3 4)) :title \"Cool plot\" :backend :braille)
=>
      Cool plot
6⡏⠉⠉⠉⠉⠉⠉⠉⠉⣹⠛⢍⠉⠉⠉⠉⠉⠉⠉⢹
 ⡇⠀⠀⠀⠀⠀⠀⠀⣰⠃⠀⠀⠑⢄⠀⠀⠀⠀⠀⢸
 ⡇⠀⠀⠀⠀⠀⠀⣰⠃⠀⠀⠀⠀⠀⠑⢄⠀⠀⠀⢸
 ⡇⠀⠀⠀⠀⠀⣰⠃⠀⠀⠀⠀⠀⠀⠀⠀⠑⢄⠀⢸
 ⡇⠀⠀⠀⠀⣰⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠑⢼
 ⡇⠀⠀⠀⣰⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
 ⡇⠀⠀⣰⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
 ⡇⠀⣰⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
 ⡇⣰⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
2⣷⣃⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸
 1                  3
"
										; todo add flag to toggle drawing axes
  (let ((canvas (create-canvas (first size) (second size) backend)))
	(multiple-value-bind (xminmax yminmax xlim ylim) (plot-scale-calculations data zoom)
	  (draw-chart! canvas
				   (scale-points data (cell-resolution canvas) xminmax yminmax size zoom)
				   thickness)
	  (draw-frame! canvas)
	  (print-canvas-with-labels canvas :title title
									   :xlabel xlabel
									   :ylabel ylabel
									   :xlim xlim
									   :ylim ylim
									   :fmt-stream (if as-string
													   (make-array 0
																   :element-type 'character
																   :fill-pointer 0)
													   t)))))

(defun plot-fun (fn min max &optional (step 1) (backend :blocks))
  "Plot the single-input function FN over the given interval.

Examples:
(plot-fun #'sin -4 4 0.1 :braille)
=>
               0.99957365⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⠏⠙⢏⠉⠉⠉⢹
                         ⣇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡎⠀⠀⠘⡆⠀⠀⢸
                         ⡟⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⡸⠀⠀⠀⠀⢹⠀⠀⢸
                         ⡇⢳⠀⠀⠀⠀⠀⠀⠀⠀⢀⠇⠀⠀⠀⠀⠀⡇⠀⢸
                         ⡇⠈⡆⠀⠀⠀⠀⠀⠀⠀⡼⠀⠀⠀⠀⠀⠀⢸⠀⢸
#<FUNCTION SIN>          ⡇⠀⢱⠀⠀⠀⠀⠀⠀⢠⠇⠀⠀⠀⠀⠀⠀⠀⡇⢸
                         ⡇⠀⠈⡆⠀⠀⠀⠀⠀⡜⠀⠀⠀⠀⠀⠀⠀⠀⢸⣸
                         ⡇⠀⠀⢱⠀⠀⠀⠀⢠⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⢿
                         ⡇⠀⠀⠈⣇⠀⠀⠀⡎⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
               -0.9995735⣇⣀⣀⣀⣘⣄⣀⣜⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸
                         -4.0       3.9999971
                                  x
"
  (plot (mapcar (lambda (x) (list x (funcall fn x)))
				(loop for i from min to max by step
					  collect i))
		:ylabel (write-to-string fn)
		:xlabel "x"
		:backend backend))

(defun plot-multiple (data-list &key title xlabel ylabel (size '(20 10)) (thickness 1) (zoom '(1 1)) (as-string nil) (backend :blocks))
  "Plot multiple lines of data as a combined chart.
Setting *colors-enabled* to t is recommended.

DATA-LIST: a list of Nx2 list of points. (MxNx2)

TITLE: a string, printed above the plot.
XLABEL and YLABEL: strings describing the axes, printed next to the axes.
SIZE: plot size in characters as a list '(x y).
THICKNESS: line thickness.
ZOOM: how much to scale the plot as a list '(x y).
AS-STRING: if t, return plot as a string instead of printing.
BACKEND: which rendering backend to use.

Examples:
(plot-multiple '(((1 2) (2 6) (3 4))
				 ((1 3) (2 2) (3 5))) :backend :braille)
=>
6⡏⠉⠉⠉⠉⠉⠉⠉⠉⣹⠛⢍⠉⠉⠉⠉⠉⠉⠉⢹
 ⡇⠀⠀⠀⠀⠀⠀⠀⣰⠃⠀⠀⠑⢄⠀⠀⠀⠀⠀⢸
 ⡇⠀⠀⠀⠀⠀⠀⣰⠃⠀⠀⠀⠀⠀⠑⢄⠀⠀⠀⢸
 ⡇⠀⠀⠀⠀⠀⣰⠃⠀⠀⠀⠀⠀⠀⠀⠀⠑⢄⢠⢺
 ⡇⠀⠀⠀⠀⣰⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡰⠑⢼
 ⡇⠀⠀⠀⣰⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠜⠀⠀⢸
 ⡇⠀⠀⣰⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⠊⠀⠀⠀⢸
 ⣧⣀⣰⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⡰⠁⠀⠀⠀⠀⢸
 ⡇⣸⠛⠲⢤⣀⠀⠀⠀⠀⠀⢀⠜⠀⠀⠀⠀⠀⠀⢸
2⣷⣃⣀⣀⣀⣈⣙⣲⣤⣀⣠⣊⣀⣀⣀⣀⣀⣀⣀⣸
 1                  3
"
										; todo add flag to toggle drawing axes
  (let ((canvas (create-canvas (first size) (second size) backend)))
	(multiple-value-bind (xminmax yminmax xlim ylim) (plot-scale-calculations
													  (flatten-1 data-list) zoom)
	  (loop for data in data-list
			for color in +4bit-colormap+
			do (draw-chart! canvas
							(scale-points data
										  (cell-resolution canvas)
										  xminmax yminmax size zoom)
							thickness :color color))
	  (draw-frame! canvas)
	  (print-canvas-with-labels canvas :title title
									   :xlabel xlabel
									   :ylabel ylabel
									   :xlim xlim
									   :ylim ylim
									   :fmt-stream (if as-string
													   (make-array 0
																   :element-type 'character
																   :fill-pointer 0)
													   t)))))

(defun scatter (data &key title xlabel ylabel (size '(20 10)) (thickness 1) (zoom '(0.8 0.8)) (as-string nil) (backend :blocks))
  "Plot data as a scatterplot.

DATA: an Nx2 list of points.

TITLE: a string, printed above the plot.
XLABEL and YLABEL: strings describing the axes, printed next to the axes.
SIZE: plot size in characters as a list '(x y).
THICKNESS: circle size.
ZOOM: how much to scale the plot as a list '(x y).
AS-STRING: if t, return plot as a string instead of printing.
BACKEND: which rendering backend to use.

Examples:
(scatter '((1 2) (2 6) (3 3) (4 4)) :backend :braille)
=>
6.5⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
   ⡇⠀⠀⠀⠀⠀⠀⠀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠈⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⠀⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠋⠀⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠠⡦⠀⠀⠀⠀⠀⠀⢸
   ⡇⠀⠀⢀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
   ⡇⠀⠀⠙⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
1.5⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸
   0.625          4.375
"
										; todo add flag to toggle drawing axes
  (let ((canvas (create-canvas (first size) (second size) backend)))
	(multiple-value-bind (xminmax yminmax xlim ylim) (plot-scale-calculations data zoom)
	  (draw-scatter! canvas
					 (scale-points data (cell-resolution canvas) xminmax yminmax size zoom)
					 thickness)
	  (draw-frame! canvas)
	  (print-canvas-with-labels canvas :title title
									   :xlabel xlabel
									   :ylabel ylabel
									   :xlim xlim
									   :ylim ylim
									   :fmt-stream (if as-string
													   (make-array 0
																   :element-type 'character
																   :fill-pointer 0)
													   t)))))

(defun scatter-multiple (data-list &key title xlabel ylabel (size '(20 10)) (thickness 1) (zoom '(0.8 0.8)) (as-string nil) (backend :blocks))
  "Plot multiple sets of data as a combined scatterplot.
Setting *colors-enabled* to t is recommended.

DATA-LIST: a list of Nx2 list of points. (MxNx2)

TITLE: a string, printed above the plot.
XLABEL and YLABEL: strings describing the axes, printed next to the axes.
SIZE: plot size in characters as a list '(x y).
THICKNESS: circle size.
ZOOM: how much to scale the plot as a list '(x y).
AS-STRING: if t, return plot as a string instead of printing.
BACKEND: which rendering backend to use.

Examples:
(scatter-multiple '(((1 2) (2 6) (3 3) (4 4))
                    ((2 4) (4 3))) :backend :braille)
=>
6.5⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
   ⡇⠀⠀⠀⠀⠀⠀⠀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠈⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠀⡀⠀⠀⠀⠀⠀⠀⠀⡀⠀⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠈⠋⠀⠀⠀⠀⠀⠀⠈⠋⠀⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠠⡦⠀⠀⠠⡦⠀⠀⢸
   ⡇⠀⠀⢀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
   ⡇⠀⠀⠙⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸
1.5⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸
   0.625          4.375
"
										; todo add flag to toggle drawing axes
  (let ((canvas (create-canvas (first size) (second size) backend)))
	(multiple-value-bind (xminmax yminmax xlim ylim) (plot-scale-calculations
													  (flatten-1 data-list) zoom)
	  (loop for data in data-list
			for color in +4bit-colormap+
			do (draw-scatter! canvas
							  (scale-points data
											(cell-resolution canvas)
											xminmax yminmax size zoom)
							  thickness :color color))
	  (draw-frame! canvas)
	  (print-canvas-with-labels canvas :title title
									   :xlabel xlabel
									   :ylabel ylabel
									   :xlim xlim
									   :ylim ylim
									   :fmt-stream (if as-string
													   (make-array 0
																   :element-type 'character
																   :fill-pointer 0)
													   t)))))
