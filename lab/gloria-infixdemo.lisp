#|
Warning:
load infix.lisp before this file!
|#

#n(
	(defun foo (x y)
		(let	(
				(diff (x - y))
				(sum (x + y)))
			(print 'sum)
			(print sum)
			(print 'diff)
			(print diff)
			(print 'returns-product-of-sum-and-diff)
			(sum * diff))))

#n(
	(defun bar (x y)
		(if (x == y)
			(print 'x-and-y-are-equal)
			(print 'x-and-y-are-unequal))))

#n(
	(defun quadratic+ (a b c)
		(;pseudo-infix
			((- b) + (sqrt (b ** 2 - 4 * a * c)))
			/
			(2 * a)
		);pseudo-infix
	)
)

#n(
	(defun quadratic- (a b c)
		(
			((- b) - (sqrt (b ** 2 - 4 * a * c)))
			/
			(2 * a)
		)
	)
)

#n(
	(defun tconc (l v)
		(car
			(if (car l)
				( (cdr l) = (cddr l) = (cons v nil) )
				( (car l) = (cdr l) = (cons v nil) ) ))))

