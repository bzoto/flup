
(require 
  "../flush+.scm"
  "../precs.scm"
  "../floyd2flush2.scm")


;; --- example 2 ---


(define rs '((S 1 S >>)
	     (S 2 a)
	     ))

(define nt '(S))
(define tr '( >> a $))


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


(run-it automa '(a >> >> >>) init)
(run-it automa '(>> >>) init)
(run-it automa '(a ) init)

