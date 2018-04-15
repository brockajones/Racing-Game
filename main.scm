(include "universe.scm")
(include "color.scm")
(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:))

(define render-circles (lambda (world sdl col)
			 (let* ([circle-a (hash-ref world 'circle-a)]
				[pos (hash-ref world 'circle-a 'pos)]
				[texture (hash-ref circle-a 'image)])
			   (render-texture sdl texture (floor (car pos)) (floor (cdr pos)) col 30 30))))


(big-bang (init-world (lambda (sdl) (make-hash 
				      (pos 0)
				      (circle-a (make-hash 
						  (image (make-circle 30 sdl #f #f 
								      (lambda (a b)
									(or 
									  (and (> a b) (< (/ a 2) b) ) 
									  (> (/ a 10) b) ))))
						  (pos (cons 50 50))
						  (vel (cons 0 0))))
				      (color 0))) 1280 720)
	  (on-draw (lambda (w sdl) 
		     (let* ([x (hash-ref w 'pos)]
			    [c (hue->rgb (floor (hash-ref w 'color)))])
		       (hash-table-update! w 'color (lambda (x) (+ 0.1 x)))
		       (set-color sdl (invert c))
		       (draw-fill-rect sdl x x 500 500)
		       (render-circles w sdl (invert c))
		       (set-color sdl c)
		       w)))
	  (stop-when (lambda (w) (> (hash-ref w 'pos) 500)))
	  (on-key (lambda (world event) (let ([m (cond [(equal? event 'up) '(0 . -1)]
						       [(equal? event 'down) '(0 . 1)]
						       [(equal? event 'left) '(-1 . 0)]
						       [(equal? event 'right) '(1 . 0)]
						       [else '(0 . 0)])]) 
					  (hash-update world 'circle-a 'pos (lambda (x) (cons (+ (car x) (car m)) 
									       (+ (cdr x) (cdr m))))))
		    (cond [(eq? 'space event) 
			   (hash-table-update! world 'pos (lambda (x) (+ 1 x))) ]) world)))
