;(define-syntax test
;  (macro-rules world ()
;	       ((_ . body)
;		`(define uni (lambda (,world)
;			       (begin ,body))))))

(define-syntax test
  (syntax-rules (init-world on-draw)
		((_ (init-world a-world) body body* ...)
		 (let ([run (lambda (world) (test world body body* ...))]) (run a-world)))
		((_ world (on-draw proc) body ...)
		 (proc (test world body ...)))
		((_ world)
		 world)))
