(label append (lambda (x y)
	(cond
		((eq x (quote ())) y)
		((quote T) (cons (car x) (append (cdr x) y)))
	)
))