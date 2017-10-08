(in-package effable-ml)

(defmacro article (dc-desc &body content)
	`(htseml:html nil
		(htseml:head nil
			(htseml:meta (attributes nil "charset" "UTF-8"))
			(htseml:meta (attributes
				nil "name" "author"
				nil "content" (concatenate 'string (person-given-name (dc-creator ,dc-desc)) " " (person-family-name (dc-creator ,dc-desc)))
				)
			)
			(htseml:meta (attributes
				nil "name" "keywords"
				nil "content" (dc-keywords ,dc-desc)
				)
			)
			(htseml:link (attributes nil "rel" "stylesheet" nil "type" "text/css" nil "href" "style.css"))
			(htseml:title nil (dc-title ,dc-desc))
		)
		(htseml:body
			(attributes
				nil "itemscope" "itemscope"
				nil "itemtype" "http://schema.org/Article"
			)
			; This is the title block, which is typically rendered on the form
			;
			;         Can Daily Mushy Pea Consumption Reduce Heart Attack Risk?
			;                            Barry Brock
			;                       University of Skegness
			;                             1978-5-16
			(htseml:header nil
				; TITLE
				(htseml:h1
					(attributes
						nil "class" "title"
						nil "itemprop" "headline"
					)
					(dc-title ,dc-desc)
				)
				; AUTHOR
        (htseml:div (attributes nil "class" "author" nil "itemscope" "http://schema.org/Person")
        (htseml:p
					(attributes
						nil "class" "author"
						nil "itemprop" "creator"
					)
					(concatenate 'string (person-given-name (dc-creator ,dc-desc)) " " (person-family-name (dc-creator ,dc-desc)))
				)
				(htseml:p
					(attributes
						nil "class" "email"
						nil "itemprop" "email"
					)
					(person-email (dc-creator ,dc-desc))
				)
				(htseml:p
					(attributes
						nil "class" "phone"
						nil "itemprop" "phone"
					)
					(person-telephone (dc-creator ,dc-desc))
				)
				(htseml:p
					(attributes
						nil "class" "github"
						nil "itemprop" "sameAs"
					)
					(person-github (dc-creator ,dc-desc))
				)
				; DATE
				(htseml:p
					(attributes
						nil "class" "date"
						nil "itemprop" "datePublished"
					)
					(dc-date ,dc-desc)
				)
			))
			; The abstract (if there is one) and then the main content follows.
			(htseml:main nil
				(if (cl:not (equalp (dc-summary ,dc-desc) ""))
					(abstract (dc-summary ,dc-desc))
      		"")
				,@content
			)
		)
	)
)

(defmacro abstract (&body content)
	`(htseml:div
		(attributes nil "class" "abstract")
		(htseml:p
			(attributes
				nil "itemprop" "about"
			)
			,@content
		)
	)
)

(defmacro section (title &body content)
	`(htseml:section
		(attributes
			nil "class" "section"
		)
		(htseml:h2 nil ,title
			(htseml:a (attributes nil "id" (concatenate 'string (short-uuid) "_" (string-sanitize ,title))))
		)
		,@content
	)
)

(defmacro ssection (title &body content)
	`(htseml:section
		(attributes	nil "class" "ssection")
		(htseml:h3 nil ,title
			(htseml:a (attributes nil "id" (concatenate 'string (short-uuid) "_" (string-sanitize ,title))))
		)
		,@content
	)
)

(defmacro sssection (title &body content)
	`(htseml:section
		(attributes
			nil "class" "sssection"
		)
		(htseml:h4 nil ,title
			(htseml:a (attributes nil "id" (concatenate 'string (short-uuid) "_" (string-sanitize ,title))))
		)
		,@content
	)
)

; For key-value pairs, such as
; Born: 12/10/1987
; Primarily semantic umami.
(defmacro value (k v)
	`(list
		(htseml:span
			(attributes
				nil "class" "key"
			)
			,k
		)
		(htseml:span
			(attributes
				nil "class" "value"
			)
			,v
		)
	)
)

(defmacro p (&body content)
	`(htseml:p
		(attributes
			nil "class" "normal"
		)
		,@content
	)
)

(defmacro em (&body content)
	`(htseml:em nil
		,@content
	)
)

(defmacro cite (what)
	`(htseml:cite nil
		,what
	)
)

(defmacro email (address)
	`(htseml:span
		(attributes
			nil "class" "email"
		)
		,address
	)
)

(defmacro verbatim (content)
	`(htseml:pre
		(attributes
			nil "class" "verbatim"
		)
		,content
	)
)

(defmacro source-code (lang content)
	`(htseml:pre
		(attributes
			nil "class" "source-code"
		)
		(format nil "~A" ,content)
	)
)

(defmacro figure (caption &body content)
	`(htseml:figure nil
		,@content
		(htseml:figcaption nil
			(htseml:p
				(attributes
					nil "class" "caption"
				)
			,caption)
		)
	)
)

(defmacro image (alt src)
	`(htseml:img
		(attributes nil "alt" ,alt
					nil "src" ,src
		)
	)
)

(defmacro equation (caption &body content)
	`(htseml:div
		(attributes
			nil "class" "equation"
		)
		,@content
		(htseml:p
			(attributes
				nil "class" "caption"
			)
			,caption
		)
	)
)

(defmacro sidebar (&body content)
	`(htseml:aside
		(attributes
			nil "class" "sidebar"
		)
		,@content
	)
)

(defmacro caution (&body content)
	`(htseml:aside
		(attributes
			nil "class" "tip"
		)
		,@content
	)
)

(defmacro tip (&body content)
	`(htseml:aside
		(attributes
			nil "class" "tip"
		)
		,@content
	)
)

(defmacro top-matter (&body content)
	`(htseml:div
		(attributes
			nil "class" "top-matter"
		)
		,@content
	)
)

(defmacro main-matter (&body content)
	`(htseml:div
		(attributes
			nil "class" "main-matter"
		)
		,@content
	)
)

(defmacro back-matter (&body content)
	`(htseml:div
		(attributes
			nil "class" "back-matter"
		)
		,@content
	)
)

(defmacro l (&body content)
	`(list ,@content (htseml:br nil))
)

(defmacro thead (&body cols)
	(defun hlth (cell)
		(htseml:th nil cell)
	)
	`(htseml:tr nil
		(mapcar #'hlth (list ,@cols))
	)
)

(defmacro trow (&body cols)
	(defun hltd (cell)
		(htseml:td nil cell)
	)
	`(htseml:tr nil
		(mapcar #'hltd (list ,@cols))
	)
)

(defmacro table (caption &body cells)
	`(htseml:table nil
		(htseml:caption nil ,caption)
		(list ,@cells)
	)
)

(defmacro quotation (citation &body body)
	`(htseml:blockquote nil
		,@body
		(htseml:cite (attributes nil "class" "quote_cite") ,citation)
	)
)

(defmacro ul (&body content)
	`(htseml:ul nil
		,@content
	)
)

(defmacro ol (&body content)
	`(htseml:ol nil
		,@content
	)
)

(defmacro li (&body content)
	`(htseml:li nil ,@content )
)

(defmacro rule ()
	`(htseml:hr nil)
)

(defmacro ref (handle)
	`(htseml:a
		(attributes
			nil "href" (concatenate 'string "#_ref-" ,handle)
			nil "class" "ref"
		)
		,handle
	)
)

(defmacro note (handle content)
	`(list
		(htseml:aside
			(attributes
				nil "class" "note"
				nil "id" (concatenate 'string "_ref-" ,handle)
			)
			(htseml:p
				(attributes nil "class" "note")
				(htseml:span
					(attributes nil "class" "ref")
					,handle
				)
			,content
			)
		)
	)
)

(defun do-toc (article)
	(let ((sections (find-element 'section article)))
		(htseml:div (attributes nil "class" "toc")
			(htseml:ol nil
				(mapcar
					(lambda (s)
						(htseml:li nil
							(htseml:a
								(attributes nil "href" (concatenate 'string "#" (string-sanitize (cadr s))))
							)
						)
					)
				sections)
			)
		)
	)
)

(defmacro uri (href &optional text)
	(if (equalp "" text)
		`(htseml:a
			(attributes nil "href" ,href)
			,href
		)
		`(htseml:a
			(attributes nil "href" ,href)
			,href
		)
	)
)

(defun smart-bold (text &optional done '())
	(if (equalp text "")
		(reverse done)
		(multiple-value-bind (head bold tail) (utils:splice "*" text)
			(if (equalp bold "")
				(smart-bold tail (append (list head) done))
				(smart-bold tail (append (list (htseml:b nil bold) head) done))
			)
		)
	)
)

(defun serialize (in-file)
	(let ((doctype "html") (out-file (make-pathname :type "html" :defaults in-file)))
		(with-open-stream (out-handle (open out-file :direction :output :if-exists :supersede))
			(with-open-stream (in-handle (open in-file :direction :input))
				(let* ((dom (read in-handle)) (postpro-dom (eval (subst (do-toc dom) 'tableofcontents dom))))
					(princ (concatenate 'string "<!DOCTYPE " doctype ">") out-handle)
					(write-char #\linefeed out-handle)
					(seml:print-tree 1 postpro-dom out-handle)
				)
			)
		)
	)
)
