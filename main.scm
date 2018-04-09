(load "universe.scm")

(big-bang 1
	  (on-draw (lambda (x y) (sdl2:render-draw-color-set! (cdr y) (sdl2:make-color 200 255 255))
		     (display "hi")
		     (sdl2:render-fill-rect! (cdr y) (sdl2:make-rect (floor x) (floor x) 600 400))
		     (sdl2:render-draw-color-set! (cdr y) (sdl2:make-color 0 0 0)) (display x)
		     (+ 1 x))))
	  ;(stop-when (lambda (x) (> x 500)))
	  ;(on-key (lambda (world event) (+ 0.5 world))))
