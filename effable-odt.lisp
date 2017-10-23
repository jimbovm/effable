
(in-package effable-odt)

(defmacro article (dc-desc &body content)
	`(document-content
			(attributes
				"xmlns" "office" "urn:oasis:names:tc:opendocument:xmlns:office:1.0"
				"xmlns" "style" "urn:oasis:names:tc:opendocument:xmlns:style:1.0"
				"xmlns" "text" "urn:oasis:names:tc:opendocument:xmlns:text:1.0"
				"xmlns" "table" "urn:oasis:names:tc:opendocument:xmlns:table:1.0"
				"xmlns" "draw" "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"
				"xmlns" "fo" "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"
				"xmlns" "xlink" "http://www.w3.org/1999/xlink"
				"xmlns" "dc" "http://purl.org/dc/elements/1.1/"
				"xmlns" "meta" "urn:oasis:names:tc:opendocument:xmlns:meta:1.0"
				"xmlns" "number" "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0"
				"xmlns" "svg" "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0"
				"xmlns" "chart" "urn:oasis:names:tc:opendocument:xmlns:chart:1.0" "xmlns" "dr3d" "urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0"
				"xmlns" "math" "http://www.w3.org/1998/Math/MathML"
				"xmlns" "form" "urn:oasis:names:tc:opendocument:xmlns:form:1.0"
				"xmlns" "script" "urn:oasis:names:tc:opendocument:xmlns:script:1.0" "xmlns" "ooo" "http://openoffice.org/2004/office"
				"xmlns" "ooow" "http://openoffice.org/2004/writer"
				"xmlns" "oooc" "http://openoffice.org/2004/calc"
				"xmlns" "dom" "http://www.w3.org/2001/xml-events"
				"xmlns" "xforms" "http://www.w3.org/2002/xforms"
				"xmlns" "xsd" "http://www.w3.org/2001/XMLSchema"
				"xmlns" "xsi" "http://www.w3.org/2001/XMLSchema-instance"
				"xmlns" "rpt" "http://openoffice.org/2005/report"
				"xmlns" "of" "urn:oasis:names:tc:opendocument:xmlns:of:1.2"
				"xmlns" "xhtml" "http://www.w3.org/1999/xhtml"
				"xmlns" "grddl" "http://www.w3.org/2003/g/data-view#"
				"xmlns" "officeooo" "http://openoffice.org/2009/office"
				"xmlns" "tableooo" "http://openoffice.org/2009/table"
				"xmlns" "drawooo" "http://openoffice.org/2010/draw"
				"xmlns" "loext" "urn:org:documentfoundation:names:experimental:office:xmlns:loext:1.0"
				"xmlns" "field" "urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0"
				"xmlns" "formx" "urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0" "xmlns" "css3t" "http://www.w3.org/TR/css3-text/"
				"office" "version" "1.2")
			(cl:list
				(scripts nil)
				(font-face-decls nil)
				(body nil
					(text nil
						(title-block ,dc-desc)
						,@content)))))

; internal convenience macro
(defun title-block (desc)
	(let (
		(title (dc-title desc))
		(date (dc-date desc))
		(abstract (dc-summary desc))
		(given-name (person-given-name (dc-creator desc)))
		(family-name (person-family-name (dc-creator desc))))
	(cl:list
			(sodf:h (attributes "text" "style-name" "Title")	title)
			(sodf:p (attributes "text" "style-name" "Author") (cl:list given-name " " family-name))
			(sodf:p (attributes "text" "style-name" "Date") date)
			(sodf:p (attributes "text" "style-name" "Abstract Header") "Abstract")
			(sodf:p (attributes "text" "style-name" "Abstract Body") abstract))))

(defmacro section (title &body content)
  `(cl:list (sodf:h
		(attributes "text" "style-name" "Heading 1")
  	,title)
  	,@content))

(defmacro ssection (title &body content)
  `(cl:list (sodf:h
		(attributes "text" "style-name" "Heading 2")
  	,title)
  	,@content))

(defmacro sssection (title &body content)
  `(cl:list (sodf:h
		(attributes
			"text" "style-name" "Heading 3")
  	,title)
  	,@content))

(defmacro p (&body content)
  `(sodf:p
    (attributes "text" "style-name" "Text Body")
   	,@content))

(defmacro ol (&body content)
	`(sodf:list
		(attributes "text" "style-name" "List 1")
		,@content))

(defmacro ul (&body content)
	`(sodf:list
		(attributes "text" "style-name" "Numbering 1")
		,@content))

(defmacro li (&body content)
	`(sodf:list-item nil (sodf:p (attributes "text" "style-name" "Text Body") ,@content)))

; analogous to effable-ml's hltd
(defun hlcell (cell)
	(sodf:table-cell nil (sodf:p (attributes "text" "style-name" "Table Cell" "office" "value-type" "string") cell)))

(defmacro table (caption &body cells)
	`(sodf:table
		(attributes "table" "style-name" "Table")
		(cl:list
			(sodf:table-column (attributes "table" "number-columns-repeated" (cl:1- (cl:length (cl:car '(,@cells))))))
			,@cells)))

(defmacro thead (&body cols)
	`(sodf:table-header-rows nil
		(sodf:table-row (attributes "table" "style-name" "Table Heading")
			(mapcar #'hlcell (cl:list ,@cols)))))

(defmacro trow (&body cols)
	`(sodf:table-row (attributes "table" "style-name" "Table Row")
		(mapcar #'hlcell (cl:list ,@cols))))

(defmacro ruby (bottom top)
	`(sodf:ruby
		(sodf:ruby-base nil ,bottom)
		(sodf:ruby-text
			(attributes "text" "style-name" "Text Body" ,top))))

(defmacro strong (&body content)
	`(sodf:span (attributes nil "style-name" "Strong Emphasis") ,@content))

(defmacro em (&body content)
	`(sodf:span (attributes nil "style-name" "Emphasis") ,@content))

(defmacro email (address)
	`(sodf:span (attributes nil "style-name" "email")	,address))

(defmacro verbatim (lang content)
	`(sodf:p
		(attributes	"text" "style-name" "Preformatted Text")
		(format nil "~A" ,content)))

(defmacro source-code (lang content)
	`(sodf:p
		(attributes	"text" "style-name" "Source Code")
		(format nil "~A" ,content)))

(defun serialize (in-file)
	(let ((out-file (make-pathname :type "xml" :name "content")))
		(with-open-stream (out-handle (open out-file :direction :output :if-exists :supersede))
			(with-open-stream (in-handle (open in-file :direction :input))
				(let ((dom (eval (read in-handle))))
					(seml:print-tree 1 dom out-handle))))))
