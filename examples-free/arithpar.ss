#lang racket


(require 
  "../maxgr.ss" 
  "../gengra.ss")


(def=. (lambda (x)
	 (case x
       ('<< (set '>>))
       ('q (set 'r))
       (else (set)))))

(def>. (lambda (x)
	 (case x
       ('+ (set '+ '>> 'r))
       ('* (set '+ '* '>> 'r))
       ('>> (set '+ '* '>> 'r))
       ('a  (set '+ '* '>> 'r))
	   (else (set)))))

(def<. (lambda (x)
	 (case x
       ('+ (set '* '<< 'a))
       ('* (set '<< 'a))
       ('<< (set '+ '* '<< 'a))
       ('q (set '+ '* '<< 'a))
	   (else (set)))))

;(grammar-strings
; (chains->grammar (compute-chains '(* + a)) identity)
; 4)

(let ((G
        (chains->grammar (compute-chains '(* + a << >> q))
                         (lambda (N)
                           (let ((new-set (set)))
                             (set-for-each N
                                           (lambda (n)
                                             (let ((left (car n))
                                                   (right (cdr n)))
                                               (when (and (or 
                                                            (set-member? left 'a)
                                                            (set-member? left '<<))
                                                          (or
                                                            (set-member? right 'a)
                                                            (set-member? right '>>)))
                                                 (set! new-set (set-add new-set n))))))
                             new-set)))))
  (grammar-strings G 3))
