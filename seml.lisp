(in-package seml)

; THE MAIN CAST.
; 
; THE ELEMENT MACRO. The don of the whole operation. A versatile fellow who is
; capable of putting on a thin costume to play the role of any sort of
; nonempty XML element.

(defmacro element (name attributes namespace &body content)
	`(list (open-tag ,name ,attributes ,namespace) ,@content (close-tag ,name ,namespace))
)

; THE EMPTY-ELEMENT MACRO. The element macro’s trusted second in command who
; is called into service whenever empty elements need to be dealt with.

(defmacro empty-element (name attributes namespace)
	`(list (empty-tag ,name ,attributes ,namespace))
)

; THE MAKE-ELEMENT ANAMORPHIC MACRO.

(defmacro make-element (namespace name)
	(let ((tag-name name))
		`(setq ,name (defmacro ,name (attributes &body content) `(element ,,tag-name ,attributes ,,namespace ,@content)))
	)
)

(defmacro make-empty-element (namespace name)
	(let ((tag-name name))
		`(setq ,name (defmacro ,name (attributes) `(empty-element ,,tag-name ,attributes ,,namespace)))
	)
)

; THE TAG FUNCTIONS. This selection of hangers-on do the dirty work involving
; formatting tags that the element macro thinks it’s far too important to do
; itself.

; I’m the element macro’s most trusted underling. Cause of that, I can
; delegate a lot of my work, capisce?
(defun open-tag (name attributes &optional namespace)
	; This is the choke point for rubbin’ out dem errors.
	(when (not (listp attributes)) (error "Attempt to compile tag with name ~S and attributes ~S~%Could not read a well-formed alist of attributes. This probably means that an attributes alist is not being passed to a tag macro that requires them." name attributes))
	(if (null namespace)
		(concatenate 'string "<" (string-downcase (string name)) (utils:string-glue "" attributes) ">")
		(concatenate 'string "<" (string-downcase namespace) ":" (string-downcase (string name)) (utils:string-glue "" attributes) ">")
	)
)

; I have such a boring job! All I do is package up symbols as close tags! Jeez!
(defun close-tag (name &optional namespace)
	(if (null namespace)
		(concatenate 'string "</" (string-downcase (string name)) ">")
		(concatenate 'string "</" (string-downcase namespace) ":" (string-downcase (string name)) ">")
	)
)

; And sometimes I put even them guys outta a job! Badda-bing!
(defun empty-tag (name attributes &optional namespace)
	(if (null namespace)
		(concatenate 'string "<" (string-downcase (string name)) (utils:string-glue "" attributes) "/>")
		(concatenate 'string "<" (string-downcase namespace) ":" (string-downcase (string name)) (utils:string-glue "" attributes) "/>")
	)
)

; Us guys always get hired to deal with attributes. Those guys are like the
; poor relations of elements, ya dig?
(defmacro attributes (&rest lst)
	`(mapcar #'make-attribute-string (make-attribute-forms (list ,@lst)))
)

(defun make-attribute-string (triple)
	(let (
			(content (if (stringp (third triple)) (third triple) (write-to-string (third triple))))
		)
		(if (not (null (first triple)))
				(concatenate 'string " " (first triple) ":" (second triple) "='" content "'")
				(concatenate 'string " " (second triple) "='" content "'")
		)
	)
)

(defun make-attribute-forms (lst)
	(if (null lst)
		'()
		(cons (list (first lst) (second lst) (third lst)) (make-attribute-forms (cdddr lst)))
	)
)

; PERSONAE NON GRATAE
; These guys been unmade. They’re deprecated, capisce?

(defun attribute-pair (pair)
	; Explicit is better than implicit, that’s the way we woik, ya dig?
	(let (
			(name (car pair))
			(value (cdr pair))
		)
		(concatenate 'string (string-downcase (princ-to-string name)) "='" (princ-to-string value) "'")
	)
)

; Once Pair just next to me brings ’em in, I stick ’em all together,
; makes ’em nice and easy to deal with, capisce?
(defun attribute-string (l)
	(if (null l)
		nil
		(concatenate 'string " " (car l) (attribute-string (cdr l)))
	)
)


; THE PRINTER FUNCTIONS. These fellas deal with printing out trees of XML in 
; a format suitable for something other than Lisp to deal with.

(defun make-indent (depth) (make-string depth :initial-element #\tab))

(defun serialize (doctype in-file out-file)
	(with-open-stream (out-handle (open out-file :direction :output :if-exists :supersede))
		(with-open-stream (in-handle (open in-file :direction :input))
			(let ((dom (eval (read in-handle))))
				(princ (concatenate 'string "<!DOCTYPE " doctype ">") out-handle)
				(write-char #\linefeed out-handle)
				(print-tree 1 dom out-handle)
			)
		)
	)
)
	
(defun print-tree (depth tree out-file)
	(mapcar
		(lambda (elem)
			(if (atom elem)
				; It’s an atom; just print it, Fox!
				(princ elem out-file)
				; Otherwise it’s a branch of the tree, so indent it nicely.
				(progn
					; First write a number equal to ‘depth’ of tabs and a newline...
					(when (> depth 0)
						(write-char #\linefeed out-file)
						(princ (make-indent depth) out-file)
					)
					; Then print this branch, adding to the indentation level.
					(print-tree (+ 1 depth) elem out-file)
				)
			)
		)
	tree)
)

(defun cdata (content)
	(concatenate 'string "<![CDATA[" content "]]>")
)

(defun comment (content)
	(concatenate 'string "<!--" (write-to-string #\newline) "-->" (write-to-string #\newline))
)
