(label par (lambda (x)	
	(cond
		((eq x (quote ())) (quote P))
		((eq (par (cdr x)) (quote P)) (quote N))
		((quote T) (quote P))
	)
))
