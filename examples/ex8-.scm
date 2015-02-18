;; MPradella, 2010.04.23

;#lang scheme

(require "../flush+.scm")

;; --- An example ---

(define automa 
  (create-automaton
    '(S A B )  ; states
    '( a * + $)    ; symbols
    '(S) ; end states

    ;;; ---- precedence matrix ----
    '(
      (+ > $) (* > $) (+ < a) (* < a) (* > +)
      (+ > +) (+ < *) (* > *) ($ < a) 
      ($ = $) ($ < +) ($ < *) 
      (a > $)  (a > +) (a > *)
      )

    ;;; ---- delta ----
    '(

      (S a (A B))
      (A + (A))
      (A * (B))
      (A a (A B))
      (B a (B))
      (B * (B))
      (S + (A))
      (S * (B))

      (S S (S))
      (S B (S))
      (S A (S))
      (A S (S))
      (A A (A))
      (B A (A))
      (B S (S))
      (B B (B))
      )))

(assert (run-it automa '(a + a + a) 'S))
(assert (not (run-it automa '(a +) 'S)))
(assert (not (run-it automa '(a *) 'S)))
(assert (run-it automa '(a +  a) 'S))
(assert (run-it automa '(a + a * a * a )  'S))
(assert (run-it automa '( a *  a  ) 'S))
(assert (run-it automa '(  a  + a *  a + a  *  a  ) 'S))

