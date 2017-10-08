(in-package mathseml)

(defvar doctype "math PUBLIC '-//W3C//DTD MathML 2.0//EN' 'http://www.w3.org/Math/DTD/mathml2/mathml2.dtd'")
(defvar prefix nil)

; These are the primitives, which map directly to MathML elements.

(make-element prefix math)
(make-element prefix mrow)
(make-element prefix msup)
(make-element prefix msub)
(make-element prefix munderover)
(make-element prefix mi)
(make-element prefix mo)
(make-element prefix mn)
(make-element prefix mfrac)
(make-element prefix mfenced)
(make-element prefix msqrt)
(make-element prefix mroot)
(make-element prefix ms)
(make-element prefix mtext)
(make-empty-element prefix mspace)
(make-element prefix mtable)
(make-element prefix mtr)
(make-element prefix mtd)

; These ones are abstractions over basic MathML.
; After all, what good would a tool like this be if it were as cumbersome to use as authoring MathML directly?

; Basic arithmetic...

(defmacro = (&body args)
	`(reduce (lambda (lhs rhs) (list (format-term lhs) (mo nil "=") (format-term rhs))) (list ,@args) :from-end t)
)

(defmacro + (&body args)
	`(reduce (lambda (lhs rhs) (list (format-term lhs) (mo nil "+") (format-term rhs))) (list ,@args) :from-end t)
)

(defmacro - (&body args)
	`(reduce (lambda (lhs rhs) (list (format-term lhs) (mo nil "-") (format-term rhs))) (list ,@args) :from-end t)
)

(defmacro * (&body args)
	`(mrow nil
		(reduce (lambda (lhs rhs) (list (format-term lhs) (if (numberp rhs) (mo nil "&times;") (mo nil "&InvisibleTimes;")) (format-term rhs))) (list ,@args) :from-end t)
	)
)

(defmacro / (&body args)
	`(reduce (lambda (lhs rhs) (list (mfrac nil (mrow nil lhs) (mrow nil rhs)))) (list ,@args) :from-end t)
)

(defun expt (base power)
	(list (msup nil (mrow nil (format-term base)) (format-term power)))
)

(defmacro factor (fac &body args)
	`(* (format-term ,fac) (mfenced nil ,@args))
)

(defmacro root (index &body args)
	(cond
		((equal index 2) `(msqrt nil (mapcar #'format-term (list ,@args))))
		(t `(mroot nil (mapcar #'format-term (list ,@args)) (format-term ,index)))
	)
)

(defmacro abs (&body args)
	`(list (mo nil "|") ,@args (mo nil "|"))
)

; Summations and products...
(defmacro summation (from to &body args)
  `(list
		(mrow nil
      (munderover nil "&Sigma;" ; ∑
          (mrow nil (format-term ,from)) (mrow nil (format-term ,to)))
      ,@args))
)

(defmacro product (from to &body args)
  `(list
		(mrow nil
      (munderover nil "&Pi;"
          (mrow nil (format-term ,from)) (mrow nil (format-term ,to)))
      ,@args))
)

; Logical connectives and predicates...
(defmacro and (&body args)
	`(reduce (lambda (lhs rhs) (list (format-term lhs) (mo nil "&and;") (format-term rhs))) (list ,@args) :from-end t)
)

(defmacro or (&body args)
	`(reduce (lambda (lhs rhs) (list (format-term lhs) (mo nil "&or;") (format-term rhs))) (list ,@args) :from-end t)
)

(defmacro xor (&body args)
	`(reduce (lambda (lhs rhs) (list (format-term lhs) (mo nil "&oplus;") (format-term rhs))) (list ,@args) :from-end t)
)

(defun not (term)
	(list (mo nil "&not;") (mfenced (mrow nil (format-term term))))
)
(defun exists (term)
	(list (mo nil "&exist;") (mfenced (mrow nil (format-term term))))
)

(defun forall (term)
	(list (mo nil "&forall;") (mfenced (mrow nil (format-term term))))
)

(defun implies (lhs rhs)
	(list (format-term lhs) (mo nil "&rightarrow;") (format-term rhs))
)

(defun bimplies (lhs rhs)
	(list (format-term lhs) (mo nil "&leftrightarrow;") (format-term rhs))
)

; Everyone loves discrete maths, don’t they?
; Here are the set operation abstractions.

(defun is-in (lhs rhs)
	(list (mrow nil (format-term lhs) (mo nil "&isin;") (format-term rhs)))
)

(defun not-in (lhs rhs)
	(list (mrow nil (format-term lhs) (mo nil "&notin;") (format-term rhs)))
)

(defun union (lhs rhs)
	(list (mrow nil (format-term lhs) (mo nil "&cups;") (format-term rhs)))
)

(defun intersect (lhs rhs)
	(list (mrow nil (format-term lhs) (mo nil "&cap;") (format-term rhs)))
)

(defun diff (lhs rhs)
	(list (mrow nil (format-term lhs) (mo nil "&setminus;") (format-term rhs)))
)

(defun subset (lhs rhs)
	(list (mrow nil (format-term lhs) (mo nil "&sub;") (format-term rhs)))
)

(defun not-subset (lhs rhs)
	(list (mrow nil (format-term lhs) (mo nil "&nsub;") (format-term rhs)))
)

; Matrices and vectors.

(defmacro vec (sym)
	`(mrow nil (mi (attributes nil "class" "eff-vec") ,sym)))

(defmacro vector (&body args)
	`(mfenced (attributes nil "class" "eff-vector") (mtable nil
    	(mapcar
      	(lambda (term) (mtr nil (mtd nil (format-term term)))) (list ,@args)))))

(defmacro matrix (&body args)
	`(mfenced (attributes nil "class" "eff-matrix")
      (mtable nil
        ; Map function for rendering each row over args...
    		(mapcar
      		(lambda (row)
          	(mtr nil
              ; ...which in turn maps an inner function over each term (mtd).
              (mapcar (lambda (term) (mtd nil (format-term term))) row))) ,@args))))

; Aliases...
(defun integers () (mi "&integers;"))
(defun reals () (mi "&reals;"))
(defun empty-set () (mi "&empty;"))
(defun iff (lhs rhs) (bimplies lhs rhs))

; Lemme just tell you my M.O.:
; If it’s a list, it’s another expression, don’t format.
; If it’s a number, format that sucka as mn.
; If it’s a symbol or string, it’s gotta be an identifier, so format as mi, see?
(defun format-term (term)
	(cond
		((listp term) term)
		((numberp term) (mn nil term))
		((eq (type-of t) 'boolean) (mi nil term))
		((or (stringp term) (symbolp term)) (mi nil term))
	)
)
