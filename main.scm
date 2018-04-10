(use universe)

(big-bang (init-world 1)
	  (on-draw (lambda (x y) 
		     (draw-fill-rect y x x 500 500)
		     x))
	  (stop-when (lambda (x) (> x 500)))
	  (on-key (lambda (world event) (+ 1 world))))
