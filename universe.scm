(use srfi-69)
(use srfi-113)
(use srfi-128)
(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:))


(define hash-ref hash-table-ref)

(define-syntax make-hash 
  (syntax-rules ()
		((_ (key val) ... ) 
		 (alist->hash-table (list (cons (quote key) val) ...)))))

(define draw-fill-rect (lambda (sdl x y w h)
			 (sdl2:render-fill-rect! (cdr sdl) (sdl2:make-rect x y w h))))

(define set-color (lambda (sdl r g b)
		    (sdl2:render-draw-color-set! (cdr sdl) (sdl2:make-color r g b))))

(define draw-line (lambda (sdl x1 y1 x2 y2)
		    (sdl2:render-draw-line! (cdr sdl) x1 y1 x2 y2)))

(define distance2 (lambda (x1 y1 x2 y2)
		    (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))

(define render-texture (lambda (sdl texture x y #!optional color)
			 (sdl2:render-copy! (cdr sdl) texture #f 
					    (sdl2:make-rect x y 
							    (sdl2:texture-w texture) 
							    (sdl2:texture-h texture))))) 


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
		((_ (init-world init-proc) body body* ...)
		 (begin 
		   (define (make-events keyboard)
		     (make-hash (keyboard keyboard)))

		   (define (make-set) (set (make-equal-comparator)))

		   (define (get-events events quit)
		     (let ((event (sdl2:poll-event!)))
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
								       event))))])
			       (get-events events quit)]
			     [else events])))

		   (define (make-state world sdl return events) 
		     (make-hash (world  world) 
				(sdl  sdl)
				(return  return)
				(events  events)))



		   (define do-key-events (lambda (world events proc)
					   (cond [(null? events) world]
						 [else (proc (do-key-events 
							       world (cdr events) proc) (car events))])))
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
				 cons)]
			  [events (make-events (make-set))])
		     (letrec ([run (lambda (world)
				     (call/cc (lambda (return)
						(set! events (get-events events (lambda () (return world))))
						(let* ([state (make-state world sdl return events)])
						  (run (big-bang state body body* ...) )))))]) 
		       (run (init-proc sdl)))
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
		 (do-key-events (big-bang state body ...) 
				(set->list (hash-ref (hash-ref state 'events) 'keyboard)) proc))
		((_ state)
		 (hash-ref state 'world))))
