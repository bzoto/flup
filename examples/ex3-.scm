
(require 
  "../flush+.scm"
  "../floyd2flush.scm"
  "../precs.scm")


;; --- example 3 ---

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

(define pr 
    (compute-precs rs nt tr 'S))

(define automa 
  (create-automaton
    (the-states)      ; states
    (the-terminals)   ; symbols

    ;;; ---- precedence matrix ----
    pr

    ;;; ---- delta ----
    (compute-delta)

    #t)) ; variant!

(run-it automa '(a a b b + a a d d + c e f) (cons (cons 'S 1) '_))
(run-it automa '(a b + c f) (cons (cons 'S 1) '_))
(run-it automa '(a b + c e) (cons (cons 'S 1) '_))



