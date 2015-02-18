
(require 
  "../flush+.scm"
  "../precs.scm"
  "../floyd2flush.scm")


;; --- example 2 ---


(define rs '((S 1 << B)
	     (B 1 << A)
	     (A 1 a)
	     (A 2 << A)
	     ))

(define nt '(S A B))
(define tr '(<< a $))


(set-rules! rs)
(set-nonterminals! nt) 
(set-terminals! tr)  

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




(assert (run-it automa '(<< << a) (cons (cons 'S 1) '_)))
(assert (not (run-it automa '(<< a) (cons (cons 'S 1) '_))))

