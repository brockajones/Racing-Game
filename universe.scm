(define-syntax test 
  (syntax-rules () 
		((test world body ...)
		 (begin (define the-world world) (big-bang body ...)))))

(define-syntax big-bang
  (syntax-rules ()
		((_ (name other) ...)
		 (begin (cond [(memq (quote name) '(on-draw stop-when on-key)) 
				     (define name other)]) ... ))))

(define draw (lambda ()
	       (begin
		 (display world)
		 (display on-draw))))

;(define-syntax let2
;  (syntax-rules (on-draw)
;		((let2 (on-draw other) body ...)
;		 (let ([draw other]) (let2 body ...)))
;		((let2) (display draw))))

