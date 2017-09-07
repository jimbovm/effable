(in-package utils)

; THE SUPPORTING CAST.
;
;
; THE TREEP FUNCTION. A predicate, or advisor to nobler functions, which will
; evaluate to true if a list is a tree, and nil otherwhise.

(defun treep (l)
	(if (member-if #'listp l) t nil)
)

; THE MAPTREE FUNCTION. A servant who does the donkey work of mapping a
; function across a tree.

(defun maptree (func tree)
	(mapcar (lambda (elem) (if (atom elem) (funcall func elem) (maptree func elem))) tree)
)

(defun mapbranch (func tree)
	(mapcar (lambda (elem) (if (listp elem) (if (treep elem) (mapbranch func elem) (funcall func elem)) elem)) tree)
)

(defun find-element (e tree)
	(remove-if #'null
		(mapcar
			(lambda (subl) (if (and (listp subl) (eq (car subl) e)) subl nil))
		tree)
	)
)

(defun find-xpath (path tree)
	(let (
			(result (find-element (car path) tree))
		)
		(if (not (null (cdr path)))
			(find-xpath (cdr path) (car result))
			result
		)
	)
)

(defun string-sanitize (s)
	(let (
		(banned
			(list #\tab #\space #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/ #\: #\; #\{ #\} #\| #\< #\> #\= #\?)
		)
	)
		(string-downcase (substitute-if #\_ (lambda (ch) (member ch banned)) s))
	)
)

(defun normal-time-form (n) 
	(if (> 10 n) 
		(concatenate 'string 
			(string #\0)
			(write-to-string n)
		)
		(write-to-string n)
	)
)

(defun now ()
	(multiple-value-bind (second minute hour date month year day daylight-p zone) (get-decoded-time)
		(concatenate 'string
			(write-to-string year) "-"
			(write-to-string month) "-"
			(write-to-string date) " "
			(normal-time-form hour) ":"
			(normal-time-form minute) ":"
			(normal-time-form second) "+"
			(write-to-string zone)
		)
	)
)

(setq *tau* (* 2 pi))

(defun polar-to-cartesian (cx cy radius degrees)
	"Gives the ABSOLUTE cartesian coordinates of a heading (degrees) on the circumference of the circle centred on (CX,CY) with radius RADIUS."
	(let*
		(
			(degrees-normalized (/ degrees 360.0))
			(radius-normalized (/ radius 270.0))
			(radians (* degrees-normalized *tau*))
			(cosine (cos radians))
			(sine (sin radians))
		)
		(if (> 90 radius-normalized)
			(list
				(- cx (* cosine radius))
				(- cy (* sine radius))
			)
			(list
				(- cy (* sine radius))
				(- cx (* cosine radius))
			)
		)
		(if (> 270 radius-normalized)
			(list
				(- cy (* sine radius))
				(- cx (* cosine radius))
			)
			(list
				(- cx (* cosine radius))
				(- cy (* sine radius))
			)
		)
	)
)

(defun uuid ()
	(with-open-stream (bucket (open "/proc/sys/kernel/random/uuid" :direction :input))
		(remove #\| (write-to-string (read bucket)))
	)
)

(defun short-uuid ()
	(string-sanitize (subseq (remove #\- (uuid)) 0 7))
)

(defun string-glue (cur rest) (if (null rest) cur (string-glue (concatenate 'string cur (car rest)) (cdr rest))))

(defun splice (needle haystack)
	(let* (
			(start (search needle haystack))
			(found (and t start))
			(end (if found (search needle haystack :start2 (1+ start)) nil))
		)
		(if (null end)
			(values haystack "" "")
			(values (subseq haystack 0 start) (subseq haystack (1+ start) end) (subseq haystack (1+ end)))
		)
	)
)

