(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:))

(define-syntax big-bang 
  (syntax-rules (init-world on-draw stop-when)
		((_ (init-world val) body body* ...)
		 (begin 
		   (sdl2:set-main-ready!)
		   (sdl2:init! '(video))
		   (ttf:init!)
		   (current-exception-handler
		     (let ((original-handler (current-exception-handler)))
		       (lambda (exception)
			 (sdl2:quit!)
			 (original-handler exception))))
		   (let* ([window-size '(1280 720)]
			  [sdl (call-with-values 
				 (lambda () (sdl2:create-window-and-renderer! (car window-size) (cadr window-size) '()))
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
		 (begin (cond [(proc world) (return world)]) world))

		((_ world sdl return)
		 world)))

(big-bang (init-world 1)
	  (on-draw (lambda (x y) (sdl2:render-draw-color-set! (cdr y) (sdl2:make-color 200 255 255))
		     (sdl2:render-fill-rect! (cdr y) (sdl2:make-rect x x 600 400))
		     (sdl2:render-draw-color-set! (cdr y) (sdl2:make-color 0 0 0))
		     (+ 1 x) ))
	  (stop-when (lambda (x) (> x 1000))))
