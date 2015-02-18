#lang racket

(require "../maxgr.ss" 
	 "../gengra.ss")

(def=. (lambda (x)
	   (case x
	     ('b (set 'a))
	     (else (set)))))

(def>. (lambda (x)
	 (set)))

(def<. (lambda (x)
	 (case x
	   ('b (set 'b))
	   (else (set)))))

(grammar-strings 
 (chains->grammar (compute-chains '(a b)) identity)
 10)
