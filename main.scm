(use invert)
(include "universe.scm")
(include "color.scm")
(include "math.scm")
(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:))

(define render-circles (lambda (a-world sdl col circles)
			 (cond [(not (null? circles)) 
				(let* ([circle (car circles)]
				       [pos (hash-ref a-world circle 'pos)]
				       [texture (hash-ref a-world circle 'image)]
				       [vel (hash-ref a-world circle 'vel)])
				  (render-texture sdl texture (round (car pos)) 
						  (round (cdr pos)) col 30 30)
				  (render-circles a-world sdl col (cdr circles)))])))

(define update-pos (lambda (a-world circles)
		     (cond [(null? circles) a-world]
			   [else (let* ([world (update-pos a-world (cdr circles))]
					[circle (car circles)]
					[vel (hash-ref world circle 'vel)])
				   (hash-update world circle 'pos (lambda (x) 
								    (tup-merge + x vel))))])))


(define render-track (lambda (world sdl)
		       (map (lambda (point)
			      (apply draw-line (cons sdl point)))
			    (hash-ref world 'track 'lines))))


(define render-winner (lambda (world sdl)
			(cond [(eq? (hash-ref world 'stage) 'win)
			       (let ([circle 
				       (car (sort (hash-ref world 'circles) (lambda (x y) (> (hash-ref world x 'lap) 
											(hash-ref world y 'lap)))))])
				 (render-texture sdl (hash-ref world circle 'image) 1100 380))])))



(define render-checker (lambda (sdl x y wc hc s) ;wc/hc is the number of squares
			 (define render-hor (lambda (x2 wc2)
					      (cond [(> wc2 0) 
						     (draw-fill-rect sdl x2 y s s)
						     (render-hor (+ x2 s s) 
								 (- wc2 2))])))
			 (cond [(> hc 0)
				(if  (= 0 (modulo hc 2))
				  (render-hor x wc)
				  (render-hor (+ s x) (- wc 1)))
				(render-checker sdl x (+ y s) wc (- hc 1) s)])))


(define render-finish-line (lambda (world sdl)

			     (let ([args (hash-ref world 'track 'finish-line)])
			       (apply render-checker (cons sdl args)))))

(define check-finish-line (lambda (a-world circles)
			    (cond [(null? circles) a-world]
				  [else 
				    (let* ([world (check-finish-line a-world (cdr circles))]
					   [circle (car circles)]
					   [circle-pos (hash-ref world circle 'pos)]
					   [circle-next-pos (tup-merge + circle-pos (hash-ref world circle 'vel))]
					   [finish-line (hash-ref world 'track 'finish-line)]
					   [hit-y (and (> (cdr circle-pos) (second finish-line))
						     (< (cdr circle-pos) (+ (second finish-line) 
									    (* (fourth finish-line) 
									       (fifth finish-line)))))]
					   [max-lap (apply max 
							   (map 
							     (lambda (c) (hash-ref world c 'lap)) 
							     (hash-ref world 'circles)))])
				      (cond 
					[(and hit-y 
					      (< (car circle-pos) (first finish-line))
					      (>= (car circle-next-pos) (first finish-line)))
					 (print "ahead")
					 (hash-update world circle 'lap (lambda (x) (+ 1 x)))]
					[(and hit-y 
					      (>= (car circle-pos) (first finish-line))
					      (< (car circle-next-pos) (first finish-line)))
					 (print "behind")
					 (hash-update world circle 'lap (lambda (x) (- x 1)))]
					[(= (hash-ref world 'track 'laps) (add1 max-lap)) 
					 (hash-set world "Final Lap" 'text)]
					[(= (hash-ref world 'track 'laps) max-lap)
					 (hash-set world 'win 'stage)]
					[else
					  (if (< (- (current-seconds) (hash-ref world 'start-time)) 10)
					    world
					    (hash-set world
						      (string-append "Lap " 
								     (number->string 
								       (inexact->exact max-lap)))
						      'text))]))])))

(define reflection (lambda (vec line d-line)
		     (let* ([line-angle (atan (- (second line) (fourth line))
					      (- (first line) (third line)))]
			    [d-angle (atan (-  (fourth d-line) (second d-line))
					   (-  (third d-line) (first d-line)))]
			    [vec-angle (atan (cdr vec) (car vec))]
			    [vec-length (distance 0 0 (car vec) (cdr vec))]
			    [final-angle (+ d-angle (- d-angle (fix-angle (+ pi vec-angle))))])
		       (cond 
			 [(>= (/ pi 2) (abs (- vec-angle d-angle))) vec]
			 [else (angle->vec final-angle vec-length)]))))

(define project (lambda (line-seg point)
		  (define get-angle (lambda (x)
				      (if (= 0 (car x)) (/ pi 2)  (atan (/ (cdr x) (car x))))))
		  (let* ([line-b 
			   (cons (- (third line-seg) (first line-seg)) (- (fourth line-seg) (second line-seg)))]
			 [line-a (cons (- (car point) (first line-seg)) (- (cdr point) (second line-seg)))]
			 [theta (- (get-angle line-a) (get-angle line-b))]
			 [a1-length (* (distance 0 0 (car line-a) (cdr line-a)) (cos theta))]
			 [b-distance (distance 0 0 (car line-b) (cdr line-b))]
			 [b-hat (cons (/ (car line-b) b-distance) (/ (cdr line-b) b-distance))]
			 [a1 (cons (* (car b-hat) a1-length) (* (cdr b-hat) a1-length))]
			 [a2-start (cons (+ (car a1) (first line-seg)) (+ (cdr a1) (second line-seg)))])
		    (if (or (> a1-length b-distance) #f) #f
		      (map round (list (car a2-start) (cdr a2-start) (car point) (cdr point)))))))

(define bounce (lambda (a-world circles)
		 (cond [(null? circles) a-world]
		       [else
			 (let* ([world (bounce a-world (cdr circles))]
				[circle (car circles)]
				[res (map 
				       (lambda (line) (let ([proj 
							      (project line 
								       (hash-ref world circle 'pos))])
							(cond [proj
								(list (>= 15 (apply distance proj)) line proj)]
							      [else #f])))
				       (hash-ref world 'track 'lines))]
				[leftover-result (filter (lambda (x) (and x (car x))) res)]
				[circle-pos (hash-ref world circle 'pos)]
				[bounce-decay 0.8])
			   (define check-corner (lambda (dist pos)
						  (not (null? (filter 
								(lambda (x) 
								  (< (distance2 (first x) (second x) 
										(car pos) (cdr pos)) 
								     (expt dist 2)))
								(apply append (map 
										(lambda (x) (list (take x 2) 
												  (cddr x)))
										(hash-ref world 'track 'lines))))))))
			   (cond 
			     ;Checks for hitting corner
			     [(not (null? leftover-result))
			      (hash-update (hash-update world circle 'bounce 
							(lambda (x) (second (car leftover-result)))) circle 'vel 
					   (lambda (x) 
					     (reflection x 
							 (second (car leftover-result)) 
							 (third (car leftover-result)))))]
			     ;Check if you hit a corner
			     [(and (not (hash-ref world circle 'bounce))
				   (check-corner 10 circle-pos))
			      ;(check-corner 15 (tup-merge + circle-pos (hash-ref world circle 'vel))))
			      (hash-update (hash-update world circle 'bounce (lambda (x) #t))
					   circle 'vel (lambda (x) (tup-map 
								     (lambda (y) (* -1 bounce-decay y)) x)))]
			     [(null? leftover-result) 
			      (hash-update (cond [(and (hash-ref world circle 'bounce)
						       (> (vec-distance2 (hash-ref world circle 'vel)) 1.0))
						  (hash-update world circle 'vel
							       (lambda (vel) (tup-map 
									       (lambda (x) (* bounce-decay x)) 
									       vel)))]
						 [else world]) circle 'bounce (lambda (x) #f))]
			     [else world]))])))

(big-bang (init-world (lambda (sdl) (make-hash 
				      (stage 'count-down)
				      (start-time (current-seconds))
				      (font (open-font "data/font/carbon bl.ttf" 300))
				      (text " ")
				      (track (make-hash 
					       (laps 4)
					       (lines '((640 0 0 360) 
							(640 0 1280 360) 
							(1280 360 680 720)
							(600 720 0 360)
							(640 100 100 360) 
							(640 100 1180 360) 
							(1180 360 680 620)
							(600 620 100 360)
							(600 620 680 620)
							(600 720 680 720)))
					       (finish-line '(600 620 16 20 5))))
				      (circles '(circle-a circle-b))
				      (circle-a (make-hash
						  (bounce #f)
						  (image (invert-texture (make-circle 60 sdl #f #f 
										      (lambda (a b)
											(or 
											  (and (> a b) (< (/ a 2) b))
											  (> (/ a 10) b) )))))
						  (pos (cons 620 660))
						  (vel (cons 0.0 0.0))
						  (lap 0)))
				      (circle-b (make-hash
						  (bounce #f)
						  (image (invert-texture (make-circle 60 sdl #f #f 
										      (lambda (a b)
											(and (> a b) 
											     (< (/ a 2) b))))))
						  (pos (cons 620 700))
						  (vel (cons 0.0 0.0))
						  (lap 0)))
				      (color 0))) 1280 720)
	  (on-draw (lambda (world sdl)
		     (let* ( [c (hue->rgb (floor (hash-ref world 'color)))])
		       (invert-renderer (cdr sdl))
		       (render-circles world sdl '(255 255 255) (hash-ref world 'circles))
		       (set-color sdl '(255 255 255))
		       (render-finish-line world sdl)
		       (render-track world sdl)
		       (render-winner world sdl)
		       (render-texture sdl (invert-texture 
					     (make-text (hash-ref world 'font) (hash-ref world 'text) sdl)) 640 360)
		       (set-color sdl c)
		       (case (hash-ref world 'stage)
			 ['count-down 
			  (let ([num (inexact->exact (- 3 (quotient 
							    (- (current-seconds) (hash-ref world 'start-time)) 2)))])
			    (cond [(> num 0) (hash-set world (number->string num) 'text)]
				  [(= num 0) (hash-set (hash-set world 'run 'stage) "GO" 'text)]))]
			 ['run (update-pos 
				 (check-finish-line 
				   (bounce 
				     (hash-update world 'color (lambda (x) (+ 0.1 x))) 
				     (hash-ref world 'circles)) (hash-ref world 'circles)) 
				 (hash-ref world 'circles))]
			 ['win (hash-set world "Winner:   " 'text)]
			 [else world]))))
	  (on-key (lambda (a-world event)
		    (case (hash-ref a-world 'stage)
		      ['run 
		       (define check-direction (lambda (keys circle world)
						 (let ([m (cond [(equal? event (first keys)) '(0 . -1)]
								[(equal? event (second keys)) '(0 . 1)]
								[(equal? event (third keys)) '(-1 . 0)]
								[(equal? event (fourth keys)) '(1 . 0)]
								[else '(0 . 0)])])
						   (if (hash-ref world circle 'bounce)
						     world
						     (hash-update world circle 'vel
								  (lambda (vel) (tup-merge
										  (lambda (a b) 
										    (+ a (/ b 15))) vel m)))))))
		       (check-direction '(up down left right) 'circle-a 
					(check-direction '(w s a d) 'circle-b a-world))]
		      [else a-world]))))
