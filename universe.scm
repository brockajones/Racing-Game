(module universe (big-bang draw-fill-rect)
	(import scheme chicken)
	(use srfi-69)
	(use srfi-113)
	(use srfi-128)
	(use (prefix sdl2 sdl2:)
	     (prefix sdl2-ttf ttf:))
	(define-syntax make-hash 
	  (syntax-rules ()
			((_ (key val) ... ) 
			 (alist->hash-table (list (cons (quote key) val) ...)))))

	(define draw-fill-rect (lambda (sdl x y w h)
				 (sdl2:render-draw-color-set! (cdr sdl) (sdl2:make-color 200 255 255))
				 (sdl2:render-fill-rect! (cdr sdl) (sdl2:make-rect (floor x) (floor x) 600 400))
				 (sdl2:render-draw-color-set! (cdr sdl) (sdl2:make-color 0 0 0))))

	(define-syntax big-bang 
	  (syntax-rules (init-world on-draw stop-when on-key)
			((_ (init-world val) body body* ...)
			 (begin 
			   (define (make-events keyboard)
			     (make-hash (keyboard keyboard)))

			   (define (make-set) (set (make-equal-comparator)))

			   (define (get-events events)
			     (let ((event (sdl2:poll-event!)))
			       (cond [event 
				       (cond [(sdl2:keyboard-event? event)
					      (hash-table-set! events 'keyboard
							       (if (sdl2:keyboard-event-state event)
								 (set-adjoin (hash-ref events 'keyboard) 
									     (sdl2:keyboard-event-scancode event))
								 (set-delete (hash-ref events 'keyboard)
									     (sdl2:keyboard-event-scancode 
									       event))))])
				       (get-events events)]
				     [else events])))

			   (define (make-state world sdl return events) 
			     (make-hash (world  world) 
					(sdl  sdl)
					(return  return)
					(events  events)))



			   (define hash-ref hash-table-ref)

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
							(set! events (get-events events))
							(let* ([state (make-state world sdl return events)])
							  (run (big-bang state body body* ...) )))))]) 
			       (run val))
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
			 (hash-ref state 'world)))))
