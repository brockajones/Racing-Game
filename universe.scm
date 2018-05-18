(use srfi-69)
	(use srfi-113)
	(use srfi-128)
(use (prefix sdl2 sdl2:)
 (prefix sdl2-ttf ttf:))


	(define hash-ref (lambda (table . keys)
					  (cond [(> (length keys) 1) 
					   (apply hash-ref (cons (hash-table-ref table (car keys)) (cdr keys))) ]
					   [else (hash-table-ref table (car keys))])))

	(define hash-update (lambda (table . rest)
						 (let ([tb (hash-table-copy table)])
						  (cond [(> 2 (length rest)) (raise "Not enough arguments")]
						   [(>= 2 (length rest)) 
						   (hash-table-update! tb (car rest) (cadr rest))]
						   [else 
						   (hash-table-update!
							(apply hash-ref (cons tb (reverse (cddr (reverse rest)))))
							(cadr (reverse rest))
							(car (reverse rest)))]) tb )))
	
(define hash-set (lambda (table val . rest)
				  (apply hash-update (append (list table) rest (list (lambda (x) val))))))

	(define-syntax make-hash 
	 (syntax-rules ()
	  ((_ (key val) ... ) 
	   (alist->hash-table (list (cons (quote key) val) ...)))))

(define draw-fill-rect (lambda (sdl x y w h)
						(sdl2:render-fill-rect! (cdr sdl) (sdl2:make-rect x y w h))))

(define set-color (lambda (sdl col)
				   (sdl2:render-draw-color-set! (cdr sdl) (apply sdl2:make-color col) )))

(define draw-line (lambda (sdl x1 y1 x2 y2)
				   (sdl2:render-draw-line! (cdr sdl) x1 y1 x2 y2)))

(define distance2 (lambda (x1 y1 x2 y2)
				   (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))

(define render-texture (lambda (sdl texture x y #!optional color width height)
						(let ([width (if width width (sdl2:texture-w texture))]
							  [height (if height height (sdl2:texture-h texture))])
						 (if color (set! (sdl2:texture-color-mod texture) color))
						 (sdl2:render-copy! (cdr sdl) texture #f 
						  (sdl2:make-rect 
						   (- x (quotient width 2)) 
						   (- y (quotient height 2)) 
						   width 
						   height)))))

(define make-circle (lambda (radius sdl #!optional color surface comparator)
					 (let ([surf (if surface 
								  surface 
								  (sdl2:make-surface (* 2 radius) (+ (* 2 radius) 1) 32))]
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
					  (sdl2:create-texture-from-surface (cdr sdl) surf))))

	(define-syntax big-bang 
	 (syntax-rules (init-world on-draw stop-when on-key)
	  ((_ (init-world init-proc sizes ...) body body* ...)
	   (begin 
		(define (make-events keyboard mouse)
		 (make-hash (keyboard keyboard) (mouse mouse)))

		(define (make-set) (set (make-equal-comparator)))

		(define (get-events events quit)
		 (let ([event (sdl2:poll-event!)])
		  (cond [event 
		   (cond 
			[(sdl2:quit-event? event) (quit)]
			[(sdl2:keyboard-event? event)
			(hash-table-set! events 'keyboard
			 (if (sdl2:keyboard-event-state event)
			  (set-adjoin (hash-ref events 'keyboard) 
			   (sdl2:keyboard-event-scancode event))
			  (set-delete (hash-ref events 'keyboard)
			   (sdl2:keyboard-event-scancode 
				event))))]
			[(sdl2:mouse-button-event? event)
			(hash-table-set! events 'mouse
			 (cons (list 'button 
					(sdl2:mouse-button-event-state event) (sdl2:mouse-button-event-button event))
			  (cons (list 'movement 
					 (cons (sdl2:mouse-button-event-x event) (sdl2:mouse-button-event-y event)))
			   (hash-ref events 'mouse))))]
			[(sdl2:mouse-motion-event? event)
			(hash-table-set! events 'mouse 
			 (cons (list 'movement 
					(cons (sdl2:mouse-motion-event-x event) (sdl2:mouse-motion-event-y event)))
			  (hash-ref events 'mouse)))])
		   (get-events events quit)]
		   [else events])))

	(define (make-state world sdl return events) 
	 (make-hash (world  world) 
	  (sdl  sdl)
	  (return  return)
	  (events  events)))



(define do-events (lambda (world events proc)
				   (cond [(null? events) world]
					[else (proc (do-events 
							world (cdr events) proc) (car events))])))
	(sdl2:set-main-ready!)
	(sdl2:init!)
(ttf:init!)
	;Sets vsync
	(sdl2:set-hint! 'render-vsync "1")
	(current-exception-handler
	 (let ((original-handler (current-exception-handler)))
	  (lambda (exception)
	   (sdl2:quit!)
	   (original-handler exception))))
	(let* ([options (list sizes ...)]
		   [window-size (if (not (null? options)) options '(1280 720))]
		   [sdl (call-with-values 
			   (lambda () (sdl2:create-window-and-renderer! 
						   (first window-size) (second window-size)  '()))
			   cons)]
		   [events (make-events (make-set) '())])
	 (letrec ([run (lambda (world time)
					;Locks framerate at 60
					(let ([offset (- (quotient 1000 60) (- (sdl2:get-ticks) time))])
					 (sdl2:delay! (if (> offset 0) offset 0)))
					(let ([last-time (sdl2:get-ticks)])
					 (call/cc (lambda (return)
							   (set! events (get-events events (lambda () (return world))))
							   (let* ([state (make-state world sdl return events)])
								(run (big-bang state body body* ...) last-time ))))))]) 
	  (run (init-proc sdl) 0 ))
	 (sdl2:destroy-window! (car sdl)))))
((_ state (on-draw proc) body ...)
 (begin 
  (sdl2:render-clear! (cdr (hash-ref state 'sdl)))
  (let ([result (proc (big-bang state body ...) (hash-ref state 'sdl))])
   (sdl2:render-present! (cdr (hash-ref state 'sdl))) result)))
((_ state (stop-when proc) body ...)
 (let ([return (hash-ref state 'return)]) (cond [(proc (big-bang state body ...)) 
										   (return (hash-ref state 'world))]) 
  (big-bang state body ...)))
	((_ state (on-key proc) body ...)
	 (do-events (big-bang state body ...) 
	  (set->list (hash-ref state 'events 'keyboard)) proc))
	((_ state (on-mouse proc) body ...)
	 (let ([result (do-events (big-bang state body ...)
	  (hash-ref state 'events 'mouse) proc)])
	  (hash-table-set! (hash-ref state 'events) 'mouse '())
	  result))
	 ((_ state)
 (hash-ref state 'world))))
