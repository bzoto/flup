#lang racket


(require "../maxgr.ss" 
	 "../gengra.ss")

(def>. (lambda (x)
	 (case x
	   ('n (set '+ '*))
	   ('+ (set '+))
	   ('* (set '+ '*))
	   (else (set)))))

(def=. (lambda (x)
	 (set)))

(def<. (lambda (x)
	 (case x
	   ('+ (set 'n '*))
	   ('* (set 'n))
	   (else (set)))))


(let ((G
       (chains->grammar (compute-chains '(* + n))
			(lambda (N)
			  (let ((new-set (set)))
			    (set-for-each N
					  (lambda (n)
					    (let ((left (car n))
						  (right (cdr n)))
					      (when (and (set-member? left 'n)
							     (set-member? right 'n))
						(set! new-set (set-add new-set n))))))
			    new-set)))))
  (grammar-strings G 3))

;; maxgrammar:
;; (let ((G
;;        (chains->grammar (compute-chains '(* + n)) identity)))
;;   (grammar-strings G 3))
