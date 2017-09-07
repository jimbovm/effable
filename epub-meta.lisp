(in-package epub-meta)

(setq epub-namespace nil)
(setq dc-namespace "dc")

(seml:make-element epub-namespace package)
(seml:make-element epub-namespace manifest)
(seml:make-element epub-namespace metadata)
(seml:make-empty-element epub-namespace item)
(seml:make-element epub-namespace guide)
(seml:make-element epub-namespace reference)
(seml:make-element epub-namespace spine)
(seml:make-element epub-namespace idref)

(defun make-dc-element (field content &optional role)
	(if (not (equal field "contributor"))
		(seml:element field nil dc-namespace
			content
		)
		(seml:element field `(("role" . ,role)) dc-namespace
			content
		)
	)
)

(defun make-dc-metadata (desc)
	(metadata `(("xmlns:dc" . "http://purl.org/dc/elements/1.1/") ("xmlns:opf" . "http://www.idpf.org/2007/opf"))
		(make-dc-element "title" (dc-title desc))
		(make-dc-element "creator" (dc-creator desc))
		(make-dc-element "language" (dc-language desc))
		(make-dc-element "title" (dc-title desc))
		(make-dc-element "identifier" (dc-identifier desc))
	)
)

(defun make-toc-entry (pseudochapter)
	(let* ((title (cadr pseudochapter)) (sanitized (utils:string-sanitize title)) (filename (concatenate 'string sanitized ".html")))
		(item `((id . ,sanitized) (href . ,filename)))
	)
)

(defun make-toc (pseudochapters)
	(manifest nil
		(mapcar #'make-toc-entry pseudochapters)
	)
)
