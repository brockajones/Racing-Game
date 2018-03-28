(use (prefix sdl2 sdl2:)
     (prefix sdl2-ttf ttf:))

(define on-draw #f)
(define stop-when #f)
(define on-key #f)


(define-syntax big-bang 
  (syntax-rules () 
		((_ (
		((_ a-world body ...)
		 (let* ([run (lambda (world)
			       (run (big-bang body ...)))


(define start (lambda ()
		(sdl2:set-main-ready!)
		(sdl2:init! '(video))
		(ttf:init!)
		(current-exception-handler
		  (let ((original-handler (current-exception-handler)))
		    (lambda (exception)
		      (sdl2:quit!)
		      (original-handler exception))))
		(let ([window-size (cond [(= (length on-draw) 3) (cdr on-draw)]
					 [else '(640 480)])])
		(define-values (*window* *render*) (sdl2:create-window-and-renderer! 
						     (car window-size) (cadr window-size) '())))
		(run)))


;(define-syntax let2
;  (syntax-rules (on-draw)
;		((let2 (on-draw other) body ...)
;		 (let ([draw other]) (let2 body ...)))
;		((let2) (display draw))))

