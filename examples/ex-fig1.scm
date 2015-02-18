;; MPradella, 2010.04.23

;#lang scheme

(require "../flush+.scm")

;; --- An example ---

(define automa 
  (create-automaton
    '(S E)  ; states
    '(a ~a b ~b $)    ; symbols
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
      ($ = $)
      )

    ;;; ---- delta ----
    '(
      (S a (E))
      (S b (E))
      (E a (E))
      (E b (E))
      (E ~a (E))
      (E ~b (E))

      (E E (E))
      (E S (S))
      )))

(assert (run-it automa '(b ~b a b a ~a ~b ~a a ~a) 'S))
(assert (run-it automa '(b ~b a b a ~a ~b ~a ~a) 'S))

