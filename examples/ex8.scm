;; MPradella, 2010.04.23

;#lang scheme

(require "../flush+.scm")

;; --- An example ---

(define automa 
  (create-automaton
    '(S A B S1)  ; states
    '(<< >> a * + $)    ; symbols
    '(S) ; end states

    ;;; ---- precedence matrix ----
    '(
      (+ < <<) (* < <<) (+ > >>) (* > >>) 
      (+ > $) (* > $) (+ < a) (* < a) (* > +)
      (+ > +) (+ < *) (* > *) ($ < a) ($ < <<) 
      ($ = $) ($ < +) ($ < *) (<< < a) (<< = >>) 
      (>> > >>) (<< < <<) (>> > $) (>> > +) 
      (>> > *) (<< < +) (<< < *) 
      (a > $) (a > >>) (a > +) (a > *)
      )

    ;;; ---- delta ----
    '(
      (S1 a (A B))
      (S1 << (S1))

      (S >> (S))
      (S << (S))
      (S a (A B))
      (A + (A))
      (A * (B))
      (A a (A B))
      (A >> (A))
      (A << (S))
      (B a (B))
      (B * (B))
      (B >> (B A))
      (B << (S))
      (S + (A))
      (S * (B))

      (S S (S))
      (S B (S))
      (S A (S))
      (S S1 (S))
      ;(A S (S))
      (A S1 (S))
      (A A (A))
      (B A (A))
      (B B (B))
      )))

(assert (not (run-it automa '(a +) 'S1)))
(assert (run-it automa '(<< a + a >>) 'S1))
(assert (run-it automa '(a + << a + a >>) 'S1))
(assert (run-it automa '(a + a + a) 'S1))
(assert (not (run-it automa '(<< >>) 'S1)))
(assert (not (run-it automa '(a *) 'S1)))
(assert (not (run-it automa '(a + << a) 'S1)))
(assert (run-it automa '(a + a * a * a )  'S1))
(assert (run-it automa '(<< a * << a >> >>) 'S1))
(assert (run-it automa '(<< << a >> + a * << a + a >> * << a >> >>) 'S1))

