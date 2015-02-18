
(require 
  "../flush+.scm"
  "../floyd2flush2.scm"
  "../precs.scm")


;; --- example 2 ---


(define rs '((S 1 A)
	     (S 2 B)
	     (S 3 C)
	     (A 1 b A c)
	     (A 2 b c)
	     (B 1 f B d)
	     (B 2 f d)
	     (C 1 e C f b)
	     (C 2 e f b)
	     ))

(define nt '(S A B C))
(define tr '(b c f d e $))


(set-rules! rs)
(set-nonterminals! nt) 
(set-terminals! tr)  
(set-axiom! 'S)

(define pr 
    (compute-precs rs nt tr 'S))

(define automa 
  (create-automaton
    (the-states)      ; states
    (the-terminals)   ; symbols
    (end-states 'S)

    ;;; ---- precedence matrix ----
    pr

    ;;; ---- delta ----
    (compute-delta)

    #t)) ; variant!


(define init (compute-initial-states))


(assert (run-it automa '(e e e f b f b f b) init))
(assert (run-it automa '(f f f d d d) init))
(assert (run-it automa '(b c) init) )
(assert (not (run-it automa '(e f b f b f b) init)))
(assert (not (run-it automa '(f b) init) ))
(assert (not (run-it automa '(f f f d d) init))) 
(assert (run-it automa '(f f d d) init)) 


