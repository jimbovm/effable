(in-package htseml)

(defvar doctype "html")
(defvar prefix nil)
(setf generator (concatenate 'string (lisp-implementation-type) " " (lisp-implementation-version)))

; Sundries

(make-element prefix html)
(make-empty-element prefix link)
(make-empty-element prefix meta)
(make-element prefix style)
(make-element prefix title)
(make-element prefix a)
(make-empty-element prefix br)

; Block structure

(make-element prefix head)
(make-element prefix body)

(make-element prefix article)
(make-element prefix header)
(make-element prefix main)
(make-element prefix article)
(make-element prefix section)
(make-element prefix blockquote)
(make-element prefix cite)

(make-element prefix h1)
(make-element prefix h2)
(make-element prefix h3)
(make-element prefix h4)
(make-element prefix h5)
(make-element prefix h6)
(make-element prefix h7)
(make-element prefix h8)

(make-element prefix p)
(make-element prefix aside)
(make-element prefix summary)
(make-element prefix pre)
(make-element prefix code)
(make-element prefix details)

; Definitions

(make-element prefix dl)
(make-element prefix dt)
(make-element prefix dd)

; Logical divisions

(make-element prefix div)
(make-element prefix span)
(make-empty-element prefix hr)

; Tables and lists

(make-element prefix ul)
(make-element prefix ol)
(make-element prefix li)
(make-element prefix table)
(make-element prefix tr)
(make-element prefix th)
(make-element prefix td)

; Figures and images

(make-element prefix figure)
(make-element prefix figcaption)
(make-element prefix legend)
(make-empty-element prefix img)
(make-element prefix caption)

; Span-type elements

(make-element prefix strong)
(make-element prefix em)
(make-element prefix mark)
(make-element prefix b)
(make-element prefix i)

; Rubies

(make-element prefix ruby)
(make-element prefix rt)
