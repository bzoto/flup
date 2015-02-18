#lang racket


(require "../maxgr.ss" 
	 "../gengra.ss")


(def=. (lambda (x)
	 (case x
	   ('* (set 'a))
	   (else (set)))))

(def>. (lambda (x)
	 (case x
	   ('a (set '+ '*))
	   ('+ (set '+))
	   (else (set)))))

(def<. (lambda (x)
	 (case x
	   ('+ (set 'a '*))
	   (else (set)))))

;(grammar-strings
; (chains->grammar (compute-chains '(* + a)) identity)
; 4)

(let ((G
       (chains->grammar (compute-chains '(* + a))
			(lambda (N)
			  (let ((new-set (set)))
			    (set-for-each N
					  (lambda (n)
					    (let ((left (car n))
						  (right (cdr n)))
					      (when (and (set-member? left 'a)
							     (set-member? right 'a))
						(set! new-set (set-add new-set n))))))
			    new-set)))))
  (grammar-strings G 3))
