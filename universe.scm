(define-syntax big-bang
  (syntax-rules ()
	       ((_ (name other) ...)
		(begin (cond [(memq (quote name) '(on-draw stop-when on-key)) 
						    (define name other)]) ...))))
