(define-syntax big-bang
  (syntax-rules ()
	       ((_ (on-draw other) ...)
		(begin (define on-draw other) ... ))))

