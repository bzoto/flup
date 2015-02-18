#lang racket

(require 
 "../maxgr.ss"
 "../gengra.ss"
 )

(def=. (lambda (x)
	 (case x
	   ('call (set 'ret))
	   (else (set)))))

(def>. (lambda (x)
	 (case x
	   ('call (set 'int))
	   ('ret  (set 'call 'ret 'int))
	   ('int  (set 'call)))))
	  

(def<. (lambda (x)
	 (case x
	   ('call (set 'call))
	   ('ret  (set))
	   ('int  (set 'ret 'int)))))
	  

(let ((G
       (chains->grammar (compute-chains '(call int ret))
			(lambda (N)
			  (let ((new-set (set)))
			    (set-for-each N
					  (lambda (n)
					    (let ((left (car n))
						  (right (cdr n)))
					      (when (or (not (set-member? left 'ret))
							(and (equal? left (set 'ret))
							     (equal? right (set 'ret))))
						(set! new-set (set-add new-set n))))))
			    new-set)))))
  (grammar-strings G 2))
  

;; maxgrammar:
; (let ((G
;        (chains->grammar (compute-chains '(call int ret)) identity)))
;   (grammar-strings G 3))


				   
				 


  
