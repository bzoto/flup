
(require 
  "../flush+.scm"
  "../floyd2flush2.scm"
  "../precs.scm")


;; --- example 3 ---

;(toggle-debug!)

(define rs '((S 1 C)
	     (C 1 D + C e)
	     (C 2 B + C f)
	     (C 3 c)
	     (D 1 a D d)
	     (D 2 a d)
	     (B 1 a B b)
	     (B 2 a b)))

(define nt '(S B D C))
(define tr '(a b c d e f + $))

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


(run-it automa '(a a b b + a a d d + c e f) init)
(run-it automa '(a a b b + a a b b + c f f) init)


(assert (run-it automa '(a a b b + a a d d + c e f) init))
(assert (run-it automa '(a a b b + a a b b + c f f) init))
(assert (not (run-it automa '(a a b b + a a b b + c e f) init)))
(assert (not (run-it automa '(a a b b + a a d d d + c e f) init)))
(assert (not (run-it automa '(a a a b b + a d + c e f) init)))

