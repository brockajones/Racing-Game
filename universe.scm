(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:))

(define get-events-proc (lambda (event?)
			  (letrec ([get (lambda (match other)
					  (let ([current (sdl2:poll-event!)])
					    (cond [(not current) (begin (map sdl2:push-event! other)  match)]
						  [(event? current) (get (cons current match) other)]
						  [else (get match (cons current other))])))]) 
			    (lambda () (get '() '())))))

(define get-key-events! (get-events-proc sdl2:keyboard-event?))


(define-syntax big-bang 
  (syntax-rules (init-world on-draw stop-when on-key)
		((_ (init-world val) body body* ...)
		 (begin 
		   (sdl2:set-main-ready!)
		   (sdl2:init!)
		   (ttf:init!)
		   (current-exception-handler
		     (let ((original-handler (current-exception-handler)))
		       (lambda (exception)
			 (sdl2:quit!)
			 (original-handler exception))))
		   (let* ([window-size '(1280 720)]
			  [sdl (call-with-values 
				 (lambda () (sdl2:create-window-and-renderer!  1280 720 '()))
				 cons)])
		     (letrec ([run (lambda (world)
				     (call/cc (lambda (return)
						(run (big-bang world sdl return body body* ...)))))]) 
		       (run val))
		     (sdl2:destroy-window! (car sdl)))))
		((_ world sdl return (on-draw proc) body ...)
		 (begin 
		   (sdl2:render-clear! (cdr sdl))
		   (let ([result (proc (big-bang world sdl return body ...) sdl)])
		     (sdl2:render-present! (cdr sdl)) result)))
		((_ world sdl return (stop-when proc) body ...)
		 (begin (cond [(proc (big-bang world sdl return body ...)) (return world)]) world))
		((_ world sdl return (on-key proc) body ...)
		    (proc  (big-bang world sdl return body ...) (get-key-events!)))
		((_ world sdl return)
		 world)))

(big-bang (init-world 1)
	  (on-draw (lambda (x y) (sdl2:render-draw-color-set! (cdr y) (sdl2:make-color 200 255 255))
		     (sdl2:render-fill-rect! (cdr y) (sdl2:make-rect (floor x) (floor x) 600 400))
		     (sdl2:render-draw-color-set! (cdr y) (sdl2:make-color 0 0 0))
		     (+ 0.1 x) ))
	  (stop-when (lambda (x) (> x 1000)))
	  (on-key (lambda (world events) (cond [(not (null? events)) (display events)]) world)))
