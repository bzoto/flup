;; MPradella, 2010.04.23

;#lang scheme

(require "../flush+.scm")

;; --- An example ---

(define automa 
  (create-automaton
    '(S A B C A1 B1 C1)  ; states
    '(b c d e f $)    ; symbols
    '(A1 B1 C1) ; end states

    ;;; ---- precedence matrix ----
    '(($ < b) ($ < e) ($ < f) ($ = $) (b < b) (b = c) (b > f) (b > $) (f = d) (e = f) (d > d) (f < f) (e < e) (d > $) (f = b) (c > c) (c > $))

    ;;; ---- delta ----
    '(
      (S b (A))
      (S f (B))
      (S e (C))
      (A b (A))
      (A c (A A1))
      (B f (B))
      (B d (B B1))
      (C e (C))
      (C f (C))
      (C b (C C1))

      (A A (A))
      (A S (A))
      (A1 S (A1))
      
      (B B (B))
      (B S (B))
      (B1 S (B1))

      (C C (C))
      (C S (C))
      (C1 S (C1))


      )))

(run-it automa '(b b b c c c) 'S)
(run-it automa '(b b b b c c c c c) 'S)
(run-it automa '(e e f b) 'S)
(run-it automa '(e e f b f b) 'S)
(run-it automa '(e e f b f b f b) 'S)

