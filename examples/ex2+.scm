
(require 
  "../flush+.scm"
  "../floyd2flush2.scm"
  "../precs.scm")


;; --- example 2 ---

;(toggle-debug!)

(define rs '((S 1 A)
	     (S 2 B)
	     (S 3 C)
	     (A 1 A + B)
	     (A 2 A + C)
	     (A 3 B + B)
	     (A 4 B + C)
	     (A 5 C + B)
	     (A 6 C + C)
	     (B 1 B * C)
	     (B 2 C * C)
	     (C 1 << A >>)
	     (C 2 << B >>)
	     (C 3 << C >>)
	     (C 4 a)
	     ))

(define nt '(S A B C))
(define tr '(+ * << >> a $))
(define pr 
    (compute-precs rs nt tr 'S))


(set-rules! rs)
(set-nonterminals! nt)
(set-terminals! tr)
(set-axiom! 'S)

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


(define *input* '(a + a + a $))

(define init (compute-initial-states))




(run-it automa '(a + a + a) init)
(run-it automa '(<< << a >> + a * << a + a >> * << a >> >>) 
	init)
(run-it automa '(a + a * a * a )  init)


(assert (run-it automa '(a + a + a) init))
(assert (run-it automa '(<< << a >> + a * << a + a >> * << a >> >>) 
		init))
(assert (not (run-it automa '(<< >>) init)))
(assert (not (run-it automa '(a *) init)))
(assert (not (run-it automa '(a + << a) init)))
(assert (run-it automa '(a + a * a * a )  init))


; (write pr)(newline)
;(display (length dl))(newline)
