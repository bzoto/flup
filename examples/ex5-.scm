
(require 
  "../flush+.scm"
  "../floyd2flush.scm"
  "../precs.scm")


;; --- example 2 ---


(define rs '((S 1 C)
	     (C 1 e C f b)
	     (C 2 e f b)
	     ))

(define nt '(S C))
(define tr '(f b e $))


(set-rules! rs)
(set-nonterminals! nt) 
(set-terminals! tr)  

(define pr 
    (compute-precs rs nt tr 'S))

(write pr)(newline)

(define automa 
  (create-automaton
    (the-states)      ; states
    (the-terminals)   ; symbols

    ;;; ---- precedence matrix ----
    pr

    ;;; ---- delta ----
    (compute-delta)

    #t)) ; variant!




(run-it automa '(e e e f b f b f b) (cons (cons 'S 1) '_))
(run-it automa '(e f b f b f b) (cons (cons 'S 1) '_)) ;;  !!


