(label append (lambda (x y)
	(cond
		((eq x (quote ())) y)
		((quote T) (cons (car x) (append (cdr x) y)))
	)
))

(label reverse (lambda (x)
	(cond
		((atom x) x)
		((quote T) (append (reverse (cdr x)) (cons (car x) (quote ()))))
	)
))