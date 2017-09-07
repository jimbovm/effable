(in-package effable-epub)

; This is simply semantic umami at present.
; The output book chapters represent the elements of the list.
(defmacro book (desc &body content)
	(setf book-title `(dc-title ,desc))
	`(list ,desc ,@content)
)

; This is for totally handcrafted title pages.
; There is no standard title page that fits any and every book.
; LaTeX users learn this the hard way.
(defmacro title-page (&body content)
	`(htseml:html nil
		(htseml:head nil
			(htseml:title nil ,book-title)
			(htseml:link (attributes
							nil "rel" "stylesheet"
							nil "href" "style.css"))
			(htseml:meta (attributes
				nil "charset" "utf-8")
			)
		)
		(htseml:body nil
			(htseml:main nil
				(htseml:div (attributes
					nil "class" "title-page")
					,@content
				)
			)
		)
	)
)


(defmacro obverse (&body content)
	`(htseml:html nil
		(htseml:head nil
			(htseml:title nil ,book-title)
			(htseml:link (attributes
				nil "rel" "stylesheet"
				nil "href" "style.css")
			)
			(htseml:meta (attributes
				nil "charset" "utf-8")
			)
		)
		(htseml:body nil
			(htseml:main nil
				(htseml:div (attributes
					nil "class" "obverse")
					,@content
				)
			)
		)
	)
)

(defmacro chapter (title &rest content)
	`(htseml:html nil
		(htseml:head nil
			(htseml:title nil ,book-title)
			(htseml:link (attributes
				nil "rel" "stylesheet"
				nil "href" "style.css")
			)
			(htseml:meta (attributes
				nil "charset" "utf-8"
				)
			)
		)
		(htseml:body nil
			(htseml:header nil
				(htseml:h1 (attributes
					nil "class" "chapter-heading")
				,title)
			)
			(htseml:main nil
				(htseml:div (attributes
					nil "class" "chapter-body")
				)
					,@content
			)
		)
	)
)

(defmacro appendix (title &rest content)
	`(htseml:html nil
		(htseml:head nil
			(htseml:title nil ,book-title)
			(htseml:link (attributes
				nil "rel" "stylesheet"
				nil "href" "style.css"
			))
			(htseml:meta (attributes
				nil "charset" "utf-8")
			)
		)
		(htseml:body nil
			(htseml:header nil
				(htseml:h1 (attributes
					nil "class" "appendix-heading")
				,title)
			)
			(htseml:main
				,@content
			)
		)
	)
)

(defmacro pseudochapter (title &rest content)
	`(htseml:html
		(htseml:head
			(htseml:title ,book-title)
			(htseml:link (attributes
				nil "rel" "stylesheet"
				nil "href" "style.css"
			))
			(htseml:meta (attributes
				nil "charset" "utf-8")
			)
		)
		(htseml:body
			(htseml:header
				(htseml:h1 (attributes
					nil "class" "chapter-heading")
				,title)
			)
			(htseml:main
				,@content
			)
		)
	)
)

; All we’re really doing here is calling standard SEML serialization
; on each pseudo-chapter (which includes the title page, obverse, etc.)
(defun serialize (in-file)
	; Warning! Contains explicit state!
	(defun do-chapter (chapter file-chapnum prefix)
		(ensure-directories-exist (directory-namestring (concatenate 'string prefix "/")))
		(let ((filename (concatenate 'string prefix "/ch" (write-to-string file-chapnum) ".html")))
			(with-open-stream (this-out-file (open filename :direction :output :if-exists :supersede))
				(write-line "<!DOCTYPE html>" this-out-file)
				(seml:print-tree 1 chapter this-out-file)
			)
		)
	)
	(with-open-stream (in-handle (open in-file :direction :input))
		(let* ((raw (eval (read in-handle))) (description (car raw)) (content (cdr raw)))
			(setf chapnum 1)
			(mapcar (lambda (c) (progn (do-chapter c chapnum (string-sanitize (dc-title description))) (setf chapnum (+ chapnum 1)))) content)
		)
	)
)
