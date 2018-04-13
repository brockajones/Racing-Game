(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:))

(define distance2 (lambda (x1 y1 x2 y2)
		    (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))

(define make-circle (lambda (radius render #!optional color surface comparator)
		      (let ([surf (if surface surface (sdl2:make-surface (* 2 radius) (+ (* 2 radius) 1) 32))]
			    [color (if color color (sdl2:make-color 255 255 255))]
			    [comparator (if comparator comparator >)])
			(sdl2:lock-surface! surf)
			(do ((i 0 (+ 1 i)))
			  ((> i (* 4 radius radius)))
			  (let ([x (modulo i (* 2 radius))]
				[y (quotient i (* 2 radius))])
			    (cond [(comparator (* radius radius) (distance2 radius radius x y))
			  (sdl2:surface-set! surf x y color)])))
			(sdl2:unlock-surface! surf)
			(sdl2:create-texture-from-surface render surf))))
