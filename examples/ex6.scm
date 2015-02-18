
(require 
  "../flush+.scm"
  "../precs.scm"
  "../floyd2flush2.scm")


;; --- example 2 ---

(define rs '(
	     (S 1 H)
	     (H 1 a K c)
	     (K 1 L b M)
	     (L 1 d)
	     (M 1 e)
	     ))

(define nt '(S H K L M))
(define tr '(a b c d e $))


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


(assert (run-it automa '(a d b e c) init))
(assert (not (run-it automa '(a d b c) init)))

