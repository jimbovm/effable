(in-package epub-meta)

(setq *epub-namespace* nil)
(setq *dc-namespace* "dc")

(make-element *epub-namespace* package)
(make-element *epub-namespace* manifest)
(make-element *epub-namespace* metadata)
(make-empty-element *epub-namespace* item)
(make-element *epub-namespace* guide)
(make-element *epub-namespace* reference)
(make-element *epub-namespace* spine)
(make-element **epub-namespace** idref)

(make-element *dc-namespace* creator)
(make-element *dc-namespace* identifier)
(make-element *dc-namespace* language)
(make-element *dc-namespace* title)

(defun make-dc-metadata (desc)
	(metadata
  	(attributes
           nil "xmlns:dc" "http://purl.org/dc/elements/1.1/"
           nil "xmlns:opf" "http://www.idpf.org/2007/opf")
		(title nil (dc-title desc))
		(creator nil (concatenate 'string (person-given-name (dc-creator desc)) " " (person-family-name (dc-creator desc))))
		(language nil (dc-language desc))
		(title nil (dc-title desc))
		(identifier nil (dc-identifier desc))
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
