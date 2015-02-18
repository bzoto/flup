
(require 
  "../flush+.scm"
  "../floyd2flush2.scm"
  "../precs.scm")


;; --- example 2 ---


(define rs '((S 1 A)
	     (A 1 << A)
	     (A 2 a)
	     ))

(define nt '(S A))
(define tr '( << a $))


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


(assert (run-it automa '(<< << a) init))
(assert (run-it automa '(a) init))
(assert (not (run-it automa '(<<) init)))


