
(require 
  "../flush+.scm"
  "../floyd2flush2.scm"
  "../precs.scm")


;; --- example 3 ---

(define rs '((S 1 CD)
	     (S 2 CB)
	     (CD 1 D + CD e)
	     (CD 2 D + CB e)
	     (CD 3 c)
	     (CB 1 B + CD f)
	     (CB 2 B + CB f)
	     (CB 3 c)
	     (D 1 a D d)
	     (D 2 a d)
	     (B 1 a B b)
	     (B 2 a b)))

(define nt '(S B CD CB D))
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

(assert (run-it automa '(a a b b + a a d d + c e f) init))
(assert (run-it automa '(a b + c f) init))
(assert (not (run-it automa '(a b + c e) init)))



