(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:))
(use srfi-113)
(use srfi-128)

(define-syntax make-hash 
  (syntax-rules ()
		((_ (key val) ... ) 
		 (alist->hash-table (list (cons (quote key) val) ...)))))


(define-record events keyboard)
(define (make-set) (set (make-equal-comparator)))

(define (get-events events)
  (let ((event (sdl2:poll-event!)))
    (cond [event 
	    (cond [(sdl2:keyboard-event? event)
		   (events-keyboard-set! events 
					 (if (sdl2:keyboard-event-state event)
					   (set-adjoin (events-keyboard events) 
						       (sdl2:keyboard-event-scancode event))
					   (set-delete (events-keyboard events)
						       (sdl2:keyboard-event-scancode event)
						       )))])
	    (get-events events)]
	  [else events])))

(define do-key-events (lambda (world events proc)
			(cond [(null? events) world]
			      [else (proc (do-key-events world (cdr events) proc) (car events))])))

(define-syntax big-bang
  (syntax-rules ()
		((_ init-world (id args ...) ...)
		 (begin
		   (sdl2:set-main-ready!)
		   (sdl2:init!)
		   (ttf:init!)
		   (current-exception-handler
		     (let ((original-handler (current-exception-handler)))
		       (lambda (exception)
			 (sdl2:quit!)
			 (original-handler exception))))
		   (let* ( [window-size '(1280 720)]
			  [sdl (call-with-values 
				 (lambda () (sdl2:create-window-and-renderer!  1280 720 '()))
				 cons)]
			  [events (make-events (make-set))])
		     (letrec ([do-action (lambda (a-id lst)
				       (apply (hash-table-ref handler a-id) lst))]
			      [handler (make-hash (on-draw 
			      (lambda (proc)
				      (sdl2:render-clear! (cdr sdl))
				      (let ([result (proc ...)])
					(sdl2:render-present! (cdr sdl)) result)))
			   (stop-when (lambda (proc) (cond [(proc world)
								   (return world)]))))]
			   [run (lambda (world)
				       (call/cc (lambda (return)
						  (do-action (quote id) (list args ...))
						  (run init-world))))]) 
			 (run init-world)))
		     (sdl2:destroy-window! (car sdl))))))

(define-syntax big-bang2 
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
				 cons)]
			  [events (make-events (make-set))])
		     (letrec ([run (lambda (world)
				     (call/cc (lambda (return)
						(set! events (get-events events))
						(let* ([state (make-state world sdl return events)])
						  (run (big-bang2 state body body* ...) )))))]) 
		       (run val))
		     (sdl2:destroy-window! (car sdl)))))
		((_ state (on-draw proc) body ...)
		 (begin 
		   (sdl2:render-clear! (cdr (state-sdl state)))
		   (let ([result (proc (big-bang2 state body ...) (state-sdl state))])
		     (sdl2:render-present! (cdr (state-sdl state))) result)))
		((_ state (stop-when proc) body ...)
		 (let ([return (state-return state)]) (cond [(proc (big-bang2 state body ...)) 
							     (return (state-world state))]) 
		   (big-bang2 state body ...)))
		((_ state (on-key proc) body ...)
		 (do-key-events (big-bang2 state body ...) (set->list (events-keyboard (state-events state))) proc))
		((_ state)
		 (state-world state))))
