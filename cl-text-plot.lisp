;;;; implements a unicode canvas and plotting

(proclaim '(inline
			add-braille
			sub-braille
			make-braille
			render-braille
			xy-to-braille-index
			2d+
			2dneg
			2ddot))

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
		  (if (funcall fn obj wins)
			  (setq wins obj))
		  (if (funcall fn loses obj)
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

;;; Canvas stuff

(defparameter *braille*
  (mapcar (lambda (x) (char-code x))
		  '(#\BRAILLE_PATTERN_BLANK
			#\BRAILLE_PATTERN_DOTS-1
			#\BRAILLE_PATTERN_DOTS-2
			#\BRAILLE_PATTERN_DOTS-3
			#\BRAILLE_PATTERN_DOTS-4
			#\BRAILLE_PATTERN_DOTS-5
			#\BRAILLE_PATTERN_DOTS-6
			#\BRAILLE_PATTERN_DOTS-7
			#\BRAILLE_PATTERN_DOTS-8))
  "set of braille dot primitives.
0 is empty, 1-8 are as follows:
1 4
2 5
3 6
7 8")

(defun make-braille (&rest args)
  "returns a braille character that has the dots specified in args.
Examples:
(make-braille 1 2 3)
=> returns a char with 1,2,3 filled."
  (apply #'logior (mapcar (lambda (x) (nth x *braille*)) args)))

(defun render-braille (braillecode)
  "return the character for the corresponding braille charcode."
  (code-char braillecode))

(defun print-braille ()
  "print all the braille characters for visualization"
  (dolist (i *braille*)
	(format t "~A ~A~%" i (render-braille i))))

(defun add-braille (&rest args)
  "add braille characters

Examples:
(format t \"~A~%\" (code-char (add-braille (nth 2 *braille*) (nth 6 *braille*))))
=> prints ⠢"
  (apply #'logior args))

(defun sub-braille (x1 x2)
  "subtract x2 from x1

Examples:
(code-char (sub-braille (make-braille 1 2 3) (make-braille 2 3 4)))
=> #\BRAILLE_PATTERN_DOTS-1"
										; truth table:
										; 0 0 0
										; 0 1 0
										; 1 0 1
										; 1 1 0
  (logior (first *braille*) (logxor x1 (logand x1 x2))))

(defun xy-to-braille-index (x y)
  "Turn x y coordinates given for the 4x2 braille grid into the
corresponding braille dot number"
  (nth (+ x (* 2 y)) '(1 4 2 5 3 6 7 8)))

(defun create-canvas (x y)
  "Create an array of size x y. Actual resolution is x*2 x y*4"
  (make-array (list y x) :initial-element (nth 0 *braille*)))

(defun turn-on! (canvas x y)
  "Turn on the pixel located at x,y. Modifies canvas."
  (if (and (>= x 0) (>= y 0)
		   (< x (* 2 (array-dimension canvas 1))) (< y (* 4 (array-dimension canvas 0))))
	  (multiple-value-bind (gridx blockx) (truncate x 2)
		(multiple-value-bind (gridy blocky) (truncate y 4)
		  (setf (aref canvas gridy gridx)
				(add-braille (aref canvas gridy gridx)
							 (nth (xy-to-braille-index (truncate blockx)
													   (truncate blocky))
								  *braille*))))))
  canvas)

(defun draw-line! (canvas v1 v2 thickness)
  "Draw a line from v1 to v2 with thickness. Modifies canvas.
v1 and v2 are 2-element lists: (x,y)

Examples:
(print-canvas (draw-line! (create-canvas 6 6) '(5 5) '(10 10) 1))
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
	(mapcar (lambda (p) (turn-on! canvas (first p) (second p)))
			(loop for x from (second (first maxpoints)) to (first (first maxpoints))
				  append
				  (loop for y from (second (second maxpoints)) to (first (second maxpoints))
						when (collision-check-rect (list x y) cornerpoints)
						  collect (list x y)))))
  canvas)

(defun draw-circle! (canvas center radius)
  "Draw a circle with center and radius. Modifies canvas.

Examples:
(print-canvas (draw-circle! (create-canvas 6 6) '(5 5) 5))
"
  (let* ((maxpoints (list (list (+ (first center) radius)
								(- (first center) radius))
						  (list (+ (second center) radius)
								(- (second center) radius)))))
	(mapcar (lambda (p) (turn-on! canvas (first p) (second p)))
			(loop for x from (second (first maxpoints)) to (first (first maxpoints))
				  append
				  (loop for y from (second (second maxpoints)) to (first (second maxpoints))
						when (collision-check-circle (list x y) center radius)
						  collect (list x y)))))
  canvas)

(defun draw-frame! (canvas)
  "Draw a frame to the edge of the canvas. Modifies canvas.

Examples:
(print-canvas (draw-frame! (create-canvas 60 20)))
"
  (let ((xlen (- (* 2 (array-dimension canvas 1)) 1))
		(ylen (- (* 4 (array-dimension canvas 0)) 1)))
	(apply #'mapcar (lambda (x1 y1 x2 y2)
					  (draw-line! canvas (list x1 y1) (list x2 y2) 1))
		   (list (list 0 0 xlen 0)
				 (list 0 0 0 ylen)
				 (list xlen 0 xlen xlen)
				 (list 0 ylen ylen ylen))))
  canvas)

(defparameter *testcanvas* (create-canvas 60 20))

(defun print-canvas (canvas)
  "print the whole canvas as braille characters.

Examples:
(print-canvas (create-canvas 6 6))"
  (dotimes (i (array-dimension canvas 0))
	(dotimes (j (array-dimension canvas 1))
	  (format t "~A" (render-braille (aref canvas i j))))
	(format t "~%")))

(defun render-canvas (canvas)
  "render canvas as unicode chars"
  (array-map #'render-braille canvas))

;;; Plotting stuff

(defun print-canvas-with-labels (canvas &key title xlabel ylabel xlim ylim)
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
(print-canvas-with-labels (draw-line! (draw-frame! (create-canvas 20 10))
											   '(10 10) '(20 30) 3)
								   :title \"Hello\" :xlabel \"time\" :ylabel \"f(x)\"
								   :xlim '(0 10) :ylim '(-10 10))
"
  (let* ((canv-xlen (array-dimension canvas 1))
		 (canv-ylen (array-dimension canvas 0))
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
	(dotimes (lineno linecount)
	  (cond ((= (1+ lineno) titlecount)
			 (if title (format t "~v,@T~A~%" titlestart title))) ; title
			((< titlecount (1+ lineno) (+ titlecount canv-ylen 1))
			 (if (= lineno (truncate (/ canv-ylen 2))) ; ylabel
				 (if ylabel (format t "~A" ylabel))
				 (format t "~v,@T" ylablen))
			 (cond ((and ylim (= lineno titlecount))
					(format t "~v@A" ylimlen (second ylim))) ; upper ylim
				   ((and ylim (= (1+ lineno) (+ titlecount canv-ylen)))
					(format t "~v@A" ylimlen (first ylim))) ; lower ylim
				   (t
					(format t "~v@T" ylimlen)))
			 (dotimes (xindex (array-dimension canvas 1)) ; canvas
			   (format t "~A" (render-braille (aref canvas (- lineno titlecount) xindex))))
			 (format t "~%"))
			((= lineno (+ 1 titlecount canv-ylen)) ; xlim
			 (if xlim (format t "~v,@T~vA~v@A~%"
							  (+ ylablen ylimlen)
							  (truncate (/ canv-xlen 2))
							  (first xlim)
							  (truncate (/ canv-xlen 2))
							  (second xlim))))
			((= lineno (+ 2 titlecount canv-ylen)) ;xlabel
			 (if xlabel (format t "~v,@T~A~%" xlabstart xlabel)))))))

(defun invert-y-axis (data ymax)
  "subtract ymax from the y component of each datapoint.
data is an Nx2 list.

Examples:
(invert-y-axis '((1 2) (3 4)) 4)
=> ((1 2) (3 0))
"
  (mapcar (lambda (x) (list (first x) (- ymax (second x)))) data))

(defun scale-data (data xmin xmax ymin ymax dataxminmax datayminmax)
  "scale data so that the extrema in x and y correspond to xmax/min and ymax/min.

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

(defun plot (data &key title xlabel ylabel (size '(20 10)) (thickness 1) (zoom '(1 1)))
  "Plot data. Data needs to be an Nx2 list of points.

Examples:
(plot '((1 2) (2 6) (3 4)) :title \"Cool plot\")
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
										; todo draw axes flag
  (let* ((xminmax (best-worst #'< (mapcar #'first data)))
		 (yminmax (best-worst #'< (mapcar #'second data)))
		 (xmidpoint (/ (+ (first xminmax) (second xminmax)) 2))
		 (ymidpoint (/ (+ (first yminmax) (second yminmax)) 2))
		 (datafmt (invert-y-axis
				   (scale-data data
							   (* 2 (first size) (- 1 (first zoom)))
							   (* 2 (first size) (first zoom))
							   (* 4 (second size) (- 1 (second zoom)))
							   (* 4 (second size) (second zoom))
							   xminmax yminmax)
				   (* 4 (second size))))
		 (canvas (apply #'create-canvas size))
		 (xlim (mapcar (lambda (x)
						 (+ (/ (- x xmidpoint) (first zoom)) xmidpoint))
					   xminmax))
		 (ylim (mapcar (lambda (y)
						 (+ (/ (- y ymidpoint) (second zoom)) ymidpoint))
					   yminmax))
		 (last-pt nil))
	(dolist (pt datafmt)
	  (if last-pt
		  (draw-line! canvas last-pt pt thickness))
	  (setq last-pt pt))
	(draw-frame! canvas)
	(print-canvas-with-labels canvas :title title
									 :xlabel xlabel
									 :ylabel ylabel
									 :xlim xlim
									 :ylim ylim)))

(defun plot-fun (fn min max &optional (step 1))
  "plot the single-input function fn over the given interval.

Examples:
(plot-fun #'sin -4 4 0.1)
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
		:xlabel "x"))

(defun scatter (data &key title xlabel ylabel (size '(20 10)) (thickness 1) (zoom '(0.8 0.8)))
  "Plot data as a scatterplot. Data needs to be an Nx2 list of points.

Examples:
(scatter '((1 2) (2 6) (3 3) (4 4)))
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
										; todo draw axes flag
  (let* ((xminmax (best-worst #'< (mapcar #'first data)))
		 (yminmax (best-worst #'< (mapcar #'second data)))
		 (xmidpoint (/ (+ (first xminmax) (second xminmax)) 2))
		 (ymidpoint (/ (+ (first yminmax) (second yminmax)) 2))
		 (datafmt (invert-y-axis
				   (scale-data data
							   (* 2 (first size) (- 1 (first zoom)))
							   (* 2 (first size) (first zoom))
							   (* 4 (second size) (- 1 (second zoom)))
							   (* 4 (second size) (second zoom))
							   xminmax yminmax)
				   (* 4 (second size))))
		 (canvas (apply #'create-canvas size))
		 (xlim (mapcar (lambda (x)
						 (+ (/ (- x xmidpoint) (first zoom)) xmidpoint))
					   xminmax))
		 (ylim (mapcar (lambda (y)
						 (+ (/ (- y ymidpoint) (second zoom)) ymidpoint))
					   yminmax)))
	(dolist (pt datafmt)
	  (draw-circle! canvas pt thickness))
	(draw-frame! canvas)
	(print-canvas-with-labels canvas :title title
									 :xlabel xlabel
									 :ylabel ylabel
									 :xlim xlim
									 :ylim ylim)))
