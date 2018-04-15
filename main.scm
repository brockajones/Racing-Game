(include "universe.scm")
(include "color.scm")
(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:))

(define render-circles (lambda (world sdl col)
			 (let* ([pos (hash-ref world 'circle-a 'pos)]
				[texture (hash-ref world 'circle-a 'image)]
				[vel (hash-ref world 'circle-a 'vel)])
			   (render-texture sdl texture (floor (car pos)) (floor (cdr pos)) col 30 30)
			   (hash-update world 'circle-a 'pos (lambda (x) 
							       (cons (+ (car x) (quotient (car vel) 10))
								     (+ (cdr x) (quotient (cdr vel) 10))))))))

(define render-track (lambda (world sdl)
		       (map (lambda (point)
			      (apply draw-line (cons sdl point)))
			    (hash-ref world 'track))))

(define pi 3.141592)

(define distance (lambda (x1 y1 x2 y2)
		   (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))

(define project (lambda (line-seg point)
		  (define get-angle (lambda (x)
				      (if (= 0 (car x)) 0 (atan (/ (cdr x) (car x))))))
		  (let* ([line-b 
			   (cons (- (third line-seg) (first line-seg)) (- (fourth line-seg) (second line-seg)))]
			[line-a (cons (- (car point) (first line-seg)) (- (cdr point) (second line-seg)))]
			[theta (- (get-angle line-a) (get-angle line-b))]
			[a1-length (* (distance 0 0 (car line-a) (cdr line-a)) (cos theta))]
			[b-distance (distance 0 0 (car line-b) (cdr line-b))]
			[b-hat (cons (/ (car line-b) b-distance) (/ (cdr line-b) b-distance))]
			[a1 (cons (* (car b-hat) a1-length) (* (cdr b-hat) a1-length))]
			[a2-start (cons (+ (car a1) (first line-seg)) (+ (cdr a1) (second line-seg)))])
		    (map floor (list (car a2-start) (cdr a2-start) (car point) (cdr point))))))


		 
(big-bang (init-world (lambda (sdl) (make-hash 
				      (track '((10 10 1000 10) (1000 10 1000 400) 
							       (1000 400 10 500) (10 500 10 10)))
				      (circle-a (make-hash 
						  (image (make-circle 30 sdl #f #f 
								      (lambda (a b)
									(or 
									  (and (> a b) (< (/ a 2) b) ) 
									  (> (/ a 10) b) ))))
						  (pos (cons 50 50))
						  (vel (cons 0 0))))
				      (color 0))) 1280 720)
	  (on-draw (lambda (world sdl) 
		     (let* ( [c (hue->rgb (floor (hash-ref world 'color)))])
		       (set-color sdl (invert c))
		       (render-track world sdl)
		       (apply draw-line (cons sdl (project '(1000 400 10 500) (hash-ref world 'circle-a 'pos))))
		       (let ([return
			       (render-circles 
				 (hash-update world 'color (lambda (x) (+ 0.1 x)))
				 sdl
				 (invert c))])
			 (set-color sdl c) return ))))
	  (on-key (lambda (world event) (let ([m (cond [(equal? event 'up) '(0 . -1)]
						       [(equal? event 'down) '(0 . 1)]
						       [(equal? event 'left) '(-1 . 0)]
						       [(equal? event 'right) '(1 . 0)]
						       [else '(0 . 0)])])
					  (hash-update world 'circle-a 'vel 
						       (lambda (vel) (cons (+ (car vel) (car m)) 
									   (+ (cdr vel) (cdr m)))))))))
