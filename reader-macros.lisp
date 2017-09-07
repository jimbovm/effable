(defun make-bold (stream char)
	(declare (ignore char))
	`(htseml:b nil ,(write-string (read stream t nil t)))
)

(defun make-emph (stream char)
	(declare (ignore char))
	`(htseml:i nil ,(read stream t nil t))
)

(set-macro-character #\* #'make-bold)
(set-macro-character #\_ #'make-emph)
