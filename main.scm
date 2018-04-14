(declare (uses color))
(declare (uses universe))


(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:))

(big-bang (init-world (lambda (sdl) (make-hash (pos 1) 
					       (circle-a (make-circle 15 sdl #f #f 
								      (lambda (a b)
									(or 
									  (and (> a b) (< (/ a 2) b) ) 
									  (> (/ a 10) b) ))))
					       (col 0))))
	  (on-draw (lambda (w sdl) 
		     (let* ([x (hash-ref w 'pos)]
			    [c (hue->rgb (floor (hash-ref w 'col)))])
		       (hash-table-update! w 'col (lambda (x) (+ 0.01 x)))
		       (apply set-color (cons sdl (invert c)))
		       (draw-line sdl 0 0 x x)
		       (draw-line sdl 0 0 (+ x 500) x)
		       (draw-line sdl 0 0 x (+ x 500))
		       (draw-fill-rect sdl x x 500 500)
		       (render-texture sdl (hash-ref w 'circle-a) 50 50)
		       (apply set-color (cons sdl c))
		       w)))
	  (stop-when (lambda (w) (> (hash-ref w 'pos) 500)))
	  (on-key (lambda (world event) (cond [(eq? 'space event) 
					       (hash-table-update! world 'pos (lambda (x) (+ 1 x))) ]) world)))
