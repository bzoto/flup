;; MPradella, 2010.04.23

;#lang scheme

(require "../flush+.scm")

;; --- An example ---

(define automa 
  (create-automaton
    '(S E)  ; states
    '(a ~a b ~b c d $)    ; symbols
    '(S) ; end states

    ;;; ---- precedence matrix ----
    '(
      ($ < a)
      ($ < b)
      (a < b)
      (a < a)
      (b < a)
      (b < b)
      (b = ~b)
      (a = ~a)
      (~b > ~b)
      (~b > ~a)
      (~a > ~a)
      (~a > ~b)
      (~a > $)
      (~b > $)
      (~a < a)
      (~a < b)
      (~b < b)
      (~b < a)

      ($ < c)
      (d > $)

      (c = d)
      (a < c)
      (b < c)
      (a > d)
      (~a > d)
      (b > d)
      (~b > d)

      (c < a)
      (c < b)
      (c < ~a)
      (c < ~b)

      (d > ~a)
      (d > ~b)
      (d > a)
      (d > b)

      ($ = $)
      )

    ;;; ---- delta ----
    '(
      (S a (E))
      (S b (E))
      (E a (E))
      (E b (E))
      (S c (E))
      (E c (E))
      (E d (E))
      (E ~a (E))
      (E ~b (E))

      (E E (E))
      (E S (S))
      )))

(assert (run-it automa '(a b a ~a ~b ~a a ~a) 'S))
(assert (run-it automa '(a c b a d ~a a ~a) 'S))
(assert (not (run-it automa '(a c b a d ~a ~a a ~a) 'S)))
(assert (run-it automa '(a c b a ~a d ~a a ~a) 'S))

(assert (run-it automa '(c a b a ~a ~b d a ~a) 'S))

(assert (run-it automa '(a c a b a ~a ~b ~a b ~b d ~a) 'S))
(assert (run-it automa '(a c a b a ~a ~b ~a b d ~a) 'S))
(assert (run-it automa '(a b ~b a ~a ~a) 'S))
(assert (run-it automa '(a c a b c a d ~b ~a b d ~a) 'S))
(assert (not (run-it automa '(a c a b c a ~b ~a b d ~a d) 'S)))
