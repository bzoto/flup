
(require 
  "../flush+.scm"
  "../floyd2flush2.scm"
  "../precs.scm")


;; --- example 3 ---

(toggle-debug!)

(define rs '((S 1 E)
	     (E 1 E + T)
	     (E 2 T x a)
	     (E 3 a)
	     (T 1 T x a)
	     (T 2 a)
	     ))

(define nt '(S E T))
(define tr '(a x + $))

(set-rules! rs)
(set-nonterminals! nt) 
(set-terminals! tr)  
(set-axiom! 'S)

(define pr 
    (compute-precs rs nt tr 'S))

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

(define init (compute-initial-states))






(assert (run-it automa '(a x a + a) init))
(assert (run-it automa '(a + a x a + a) init))
(assert (not (run-it automa '(a + a x + a) init)))




