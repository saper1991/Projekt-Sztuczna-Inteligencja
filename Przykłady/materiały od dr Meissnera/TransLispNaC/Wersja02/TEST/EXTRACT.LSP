(label append (lambda (x y)
	(cond
		((eq x (quote ())) y)
		((quote T) (cons (car x) (append (cdr x) y)))
	)
))

(label extract (lambda (x)
	(cond
		((eq x (quote ())) x)
		((atom x) (cons x (quote ())))
		((quote T) (append (extract (car x)) (extract (cdr x))))
	)
))