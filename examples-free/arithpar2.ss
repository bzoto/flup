#lang racket


(require 
  "../maxgr.ss" 
  "../gengra.ss")


(def=. (lambda (x)
	 (case x
       ('<< (set '>>))
       (else (set)))))

(def>. (lambda (x)
	 (case x
       ('+ (set '+ '>>))
       ('* (set '+ '* '>>))
       ('>> (set '+ '* '>>))
       ('n  (set '+ '* '>>))
	   (else (set)))))

(def<. (lambda (x)
	 (case x
       ('+ (set '* '<< 'n))
       ('* (set '<< 'n))
       ('<< (set '+ '* '<< 'n))
	   (else (set)))))

;(grammar-strings
; (chains->grammar (compute-chains '(* + n << >>)) identity)
; 4)

(let ((G
        (chains->grammar (compute-chains '(* + n << >>))
                         (lambda (N)
                           (let ((new-set (set)))
                             (set-for-each N
                                           (lambda (n)
                                             (let ((left (car n))
                                                   (right (cdr n)))
                                                  (when (and 
                                                          (not (set-member? left '>>))
                                                          (not (set-member? right '<<))
                                                          (or 
                                                            (set-member? left 'n)
                                                            (set-member? left '<<))
                                                          (or
                                                            (set-member? right 'n)
                                                            (set-member? right '>>)))
                                                 (set! new-set (set-add new-set n))))))
                             new-set)))))
  (grammar-strings G 5))