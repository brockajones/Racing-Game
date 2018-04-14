(define hue->rgb (lambda (h)
		   (let* ([h (modulo h 360)]
			  [up-hill (floor (* (/ (modulo h 60) 60) 255))]
			 [down-hill (- 255 up-hill)])
		     (cond [(< h 60)  `(255 ,up-hill 0)]
			   [(and (>= h 60) (< h 120))  `(,down-hill 255 0)]
			   [(and (>= h 120) (< h 180))  `(0 255 ,up-hill)]
			   [(and (>= h 180) (< h 240))  `(0 ,down-hill 255)]
			   [(and (>= h 240) (< h 300))  `(,up-hill 0 255)]
			   [else  `(255 0 ,down-hill)]))))
