(define tup-map (lambda (proc tup)
		  (cons (proc (car tup)) (proc (cdr tup)))))

(define tup-merge (lambda (proc tup-a tup-b)
		    (cons (proc (car tup-a) (car tup-b))
			  (proc (cdr tup-a) (cdr tup-b)))))

(define fix-angle (lambda (rad)
		    (cond [(> rad (* 2 pi)) (fix-angle (- rad (* 2 pi)))]
			  [(< rad 0) (+ (* 2 pi) rad)]
			  [else rad])))

(define rad->deg (lambda (rad)
		   (* rad (/ 180 pi))))

(define angle->vec (lambda (d l)
		     (cons (* l (cos d))
			   (* l (sin d)))))

(define pi 3.141592)

(define distance (lambda (x1 y1 x2 y2)
		   (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))

(define extend-vec (lambda (vec len)
		     (let* ([dist (distance 0 0 (car vec) (cdr vec))]
			    [u-vec (tup-map (lambda (x) (/ x dist)) vec)])
		       (tup-map (lambda (x) (* x (+ len dist))) u-vec))))

(define add (lambda (vec-a vec-b)
	      (tup-merge + vec-a vec-b)))

(define recurse (lambda (proc . args)
				 (apply proc (cons proc args))))

(define tup->list (lambda (tup)
		    (list (car tup) (cdr tup))))
