(in-package seml-svg)

(setq *ns* nil)

(seml:make-element *ns* svg)
(seml:make-element *ns* defs)
(seml:make-element *ns* g)
(seml:make-empty-element *ns* use)
(seml:make-empty-element *ns* path)
(seml:make-empty-element *ns* polyline)
(seml:make-element *ns* line)
(seml:make-element *ns* rect)
(seml:make-element *ns* circle)
(seml:make-element *ns* text)

(defmacro pie-chart (radius dataset)
	"From DATASET, which is a list of sublists in the form (label percentage colour) create a pie chart in SVG format."
	
	; These functions are nested within the PIE-CHART macro in order to avoid clutter.
	; If you don’t approve of nested defuns, too bad for you.
	(defun make-pie-slice (legend cx cy radius start end colour)
		(let* (
			(start-polar (pc-to-angle start))
			(end-polar (pc-to-angle end))
			(start-cartesian (utils:polar-to-cartesian cx cy radius start-polar))
			(end-cartesian (utils:polar-to-cartesian cx cy radius end-polar))
			(large-arc-flag (if (> (- end-polar start-polar) 180) 1 0))
			(pathstring (concatenate 'string
				"M"
				(write-to-string cx)
				","
				(write-to-string cy) " "
				"L "
				(write-to-string (first start-cartesian)) ","
				(write-to-string (second start-cartesian)) " "
				"A"
				(write-to-string radius) "," (write-to-string radius) " "
				"0 "
				(write-to-string large-arc-flag)
				",0 "
				(write-to-string (first end-cartesian)) ","
				(write-to-string (second end-cartesian)) " "
				"Z"
			))
		)
			(g (seml:attributes 
					nil "id" legend
				)
				(path
					(seml:attributes
						nil "d" pathstring
						nil "fill" colour
					)
				)
			)
		)
	)

	(defun pc-to-angle (pc)
		(* 10 (* pc 0.36))
	)

	(defun do-slice (cx cy radius start q)
		(let* (
				(label (first q))
				(percentage (second q))
				(colour (third q))
				(end (+ start percentage))
			)
			(make-pie-slice label cx cy radius start end colour)
		)
	)

	(defun do-pie (cx cy radius start-pc data)
		(if (not (null (cadr data)))
			(let (
					(next-start-pc (+ start-pc (second (car data))))
				)
				(cons (do-slice cx cy radius start-pc (car data)) (do-pie cx cy radius next-start-pc (cdr data)))
			)
			(do-slice cx cy radius start-pc (car data))
		)
	)

	(defun do-uses (radius dataset)
		(let ((diameter (* 2 radius)))
			(cons
				(mapcar
					(lambda (entry)
						(let ((ref (concatenate 'string "#" (first entry))))
							(use 
								(seml:attributes
									"xlink" "href" ref
								)
							)
						)
					)
				dataset)
				(use 
					(seml:attributes
						nil "x" (+ diameter (/ radius 4.0))
						nil "y" radius
						"xlink" "href" "#legend"
					)
				)
			)
		)
	)

	; Tying it all together, the actual body of the PIE-CHART macro begins here.
	(let (
			(cx radius)
			(cy radius)
			(chart-uuid (utils:short-uuid))
		)
		`(svg-canvas ,(* 2 radius) ,(* 2.75 radius)
			(defs nil
				(do-pie
					,cx
					,cy
					,radius
					0
					,dataset
				)
				(g (seml:attributes nil "id" "legend")
					(make-legend ,dataset #'make-rect-label 17 ,chart-uuid)
				)
			)
			(do-uses ,radius ,dataset)
		)
	)
)

(defmacro bar-chart (height width dataset)

	(defun draw-bar (height width index total-bars entry)
		(let* (
				(label (first entry))
				(percentage (second entry))
				(colour (third entry))
				(bar-width (/ (float width) (float total-bars)))
				(bar-height (* (/ height 100) (float percentage)))
			)
			(rect 
				(seml:attributes
					nil "x" (* bar-width index)
					nil "y" (- height bar-height)
					nil "height" bar-height
					nil "width" bar-width
					nil "fill" colour
				)
			)
		)
	)

	(defun do-bars (height width dataset)
		; Warning! Contains explicit state!
		(setf index -1)
		(mapcar
			(lambda (entry) (progn (incf index) (draw-bar height width index (length dataset) entry)))
			dataset
		)
	)

	; Generate a universally unique ID for this chart.
	(setf chart-uuid (utils:short-uuid))

	`(svg-canvas ,height ,width
		(defs nil
			(g (seml:attributes 
					nil "id" (concatenate 'string "axes_" ,chart-uuid)
				)
				(line
					(seml:attributes
						nil "x1" 0
						nil "y1" 0
						nil "x2" 0
						nil "y2" ,height
						nil "stroke" "black"
						nil "stroke-width" "2px"
						nil "stroke-linecap" "round"
					)
				)
				(line
					(attributes
						nil "x1" 0
						nil "y1" ,height
						nil "x2" ,width
						nil "y2" ,height
						nil "stroke" "black"
						nil "stroke-width" "2px"
						nil "stroke-linecap" "round"
					)
				)
			)
			; Note that we are working ‘backwards’ from the bottom of the chart.
			(g 
				(attributes 
					nil "id" (make-unique-id ,chart-uuid "zonelines")
					nil "stroke" "grey"
					nil "stroke-dasharray" "2"
				)
				(line
					(attributes
						nil "id" (make-unique-id ,chart-uuid "full")
						nil "x1" 0
						nil "y1" 0
						nil "x2" ,width
						nil "y2" 0
					)
				)
				(line
					(seml:attributes
						nil "id" (make-unique-id ,chart-uuid "threequarters")
						nil "x1" 0
						nil "y1" ,(* height 0.25)
						nil "x2" ,width
						nil "y2" ,(* height 0.25)
					)
				)
				(line
					(seml:attributes
						nil "id" (make-unique-id ,chart-uuid "half")
						nil "x1" 0
						nil "y1" ,(* height 0.50)
						nil "x2" ,width
						nil "y2" ,(* height 0.50)
					)
				)
				(line
					(seml:attributes
						nil "id" (make-unique-id ,chart-uuid "onequarter")
						nil "x1" 0
						nil "y1" ,(* height 0.75)
						nil "x2" ,width
						nil "y2" ,(* height 0.75)
					)
				)
			)
			(g (attributes nil "id"	(make-unique-id ,chart-uuid "legend"))
				(make-legend ,dataset #'make-rect-label 17 ,chart-uuid)
			)
		)
		(use (seml:attributes
				"xlink" "href" (get-unique-reference ,chart-uuid "zonelines")
				nil "x" 0
				nil "y" 0
			)
		)
		(do-bars ,height ,width ,dataset)
		(use (seml:attributes
				"xlink" "href" (get-unique-reference ,chart-uuid "axes")
				nil "x" 0
				nil "y" 0
			)
		)
		(use (attributes
				"xlink" "href" (get-unique-reference ,chart-uuid "legend")
				nil "x" ,(* width 0.25)
				nil "y" ,(* height 0.25)
			)
		)
	)
)

(defun line-graph (dataset height width)

	(defun do-line (dataset lineno gap colour uuid)
		(defun make-coords (xpos gap rest done)
			(if (null rest)
				done
				(concatenate 'string (write-to-string xpos) "," (write-to-string (car rest)) " " (make-coords (+ xpos gap) gap (cdr rest) done))
			)
		)
		(g 
			(attributes 
				nil "id" (make-unique-id uuid (concatenate 'string "line" (write-to-string lineno) " "))
			)
			(polyline
				(attributes
					nil "points" (make-coords 0 gap dataset '())
					nil "stroke-width" "1.5px"
					nil "stroke" colour
					nil "fill" "none"
				)
			)
		)
	)
	
	(setf chart-uuid (utils:short-uuid))
	(svg-canvas height width
		(defs nil
			; We flip the coordinate system upside down...
			(g
				(attributes
					nil "id" (make-unique-id chart-uuid "main-frame")
					nil "transform" (concatenate 'string "translate(0," (write-to-string height) ")")
				)
				(g
					(attributes nil "transform" "scale(1,-1)")
						; TODO: extract label (car dataset) for legend
						; To decide on the gap between each point, we divide the width by the total number of points.
						(setf lineno 0)
						(mapcar (lambda (line-data)
								(do-line 
									(cddr line-data)
									(+ 1 lineno)
									(float (/ width (- (length (cddr line-data)) 1)))
									(second line-data)
									chart-uuid
								)
							)
							dataset
						)
				)
			)
			(g
				(attributes
					nil "id" (make-unique-id chart-uuid "legend")
				)
				(let ((legend-set (mapcar (lambda (sublist) (list (car sublist) (cadr sublist))) dataset)))
					(make-legend legend-set #'make-line-label 17 chart-uuid)
				)
			)
		)
		(use
			(attributes
				"xlink" "href" (get-unique-reference chart-uuid "main-frame")
				nil "fill" "none"
				nil "stroke-width" "1px"
				nil "stroke" "blue"
			)
		)
		(use
			(attributes
				"xlink" "href" (get-unique-reference chart-uuid "legend")
				nil "x" 10
				nil "y" 10
			)
		)
	)
)

(defmacro svg-canvas (height width &body content)
	`(svg
		(seml:attributes 
			;nil "xmlns:svg" "http://www.w3.org/2000/svg" 
			nil "xmlns:xlink" "http://www.w3.org/1999/xlink" 
			nil "height" ,height
			nil "width" ,width
		)
		,@content
	)
)

; This function is used for pie and bar charts.
(defun make-rect-label (entry height uuid)
	(let*
		(
			(name (first entry))
			(percentage (second entry))
			(colour (third entry))
			(label (concatenate 'string name "_label_" uuid))
		)
		(g 
			(seml:attributes 
				nil "id" label
			)
			(rect
				(seml:attributes
					nil "x" 0
					nil "y" height
					nil "width" 10
					nil "height" 10
					nil "fill" colour
					nil "stroke" "#111111"
					nil "stroke-width" "0.5px"
				)
			)
			(text
				(seml:attributes
					nil "x" 17
					nil "y" (+ height 12)
				)
				(concatenate 'string name " " (write-to-string percentage) "%")
			)
		)
	)
)

; This function is used for line graphs.
(defun make-line-label (entry height uuid)
	"For a list in the form (LABEL COLOUR), create a single legend entry suitable for line graphs with this label."
	(let*
		(
			(label (concatenate 'string (car entry) "_label_" uuid))
			(colour (cadr entry))
		)
		(g 
			(seml:attributes 
				nil "id" label
			)
			(line
				(seml:attributes
					nil "x" 0
					nil "y1" height
					nil "x2" 12
					nil "y2" height
					nil "stroke" colour
					nil "fill" "none"
					nil "stroke-width" "1.6px"
				)
			)
			(text
				(seml:attributes
					nil "x" 15
					nil "y" (+ height 3)
				)
				(car entry)
			)
		)
	)
)

(defun make-legend (dataset labelf height uuid)
	(let ((space 17))
		(if (null dataset)
			dataset
			(cons (funcall labelf (car dataset) height uuid) (make-legend (cdr dataset) labelf (+ height 17) uuid))
		)
	)
)

(defun make-unique-id (uuid name)
	(concatenate 'string name "_" uuid)
)

(defun get-unique-reference (uuid name)
	(concatenate 'string "#" name "_" uuid)
)
