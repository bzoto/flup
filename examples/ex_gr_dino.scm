
(require 
  "../flush+.scm"
  "../floyd2flush2.scm"
  "../precs.scm")


;; --- example 2 ---


(define rs '(	(S 1 A)
		(A 1 a B b)
		(A 2 c)
		(B 1 a C b)
		(C 1 a A b)
	     ))

(define nt '(S A B C))
(define tr '(a b c $))
(define pr 
    (compute-precs rs nt tr 'S))


(set-rules! rs)
(set-nonterminals! nt)
(set-terminals! tr)
(set-axiom! 'S)

(define dl (compute-delta))

(define automa 
  (create-automaton
    (the-states)      ; states
    (the-terminals)   ; symbols
    (end-states 'S)

    ;;; ---- precedence matrix ----
    pr

    ;;; ---- delta ----
    dl

    #t)) ; variant!


(define *input* '(a + a + a $))

(define init (compute-initial-states))


(assert (run-it automa '(a a a c b b b) init))
(assert (run-it automa '(c) init))
(assert (not (run-it automa '(a a c b) init)))
(assert (not (run-it automa '(a c b b ) init)))
(assert (not (run-it automa '(a a a b b b) init)))
(assert (not (run-it automa '(a c b) init)))
(assert (not (run-it automa '(a c a) init)))
(assert (not (run-it automa '(a c b) init)))
(assert (not (run-it automa '(a c c) init)))


;(write pr)(newline)
