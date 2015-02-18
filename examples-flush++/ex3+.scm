#lang racket

(require 
  "../flush++.scm"
  "../g2a.ss")


;; --- example 3 ---


(define rs '((C -> D + C e)
	     (C -> B + C f)
	     (C -> c)
	     (D -> a D d)
	     (D -> a d)
	     (B -> a B b)
	     (B -> a b)))

(define nt '(B D C))
(define tr '(a b c d e f +))

(set-rules! rs)
(set-nonterminals! nt) 
(set-terminals! tr)  
(set-axioms! '(C))


(define automa (make-automaton))
(define init (get-initial-states))




(assert (run-it automa '(a a b b + a a d d + c e f) init))
(assert (run-it automa '(a a b b + a a b b + c f f) init))
(assert (not (run-it automa '(a a b b + a a b b + c e f) init)))
(assert (not (run-it automa '(a a b b + a a d d d + c e f) init)))
(assert (not (run-it automa '(a a a b b + a d + c e f) init)))

