;; MPradella, 2010.04.23

;#lang scheme

(require "../flush+.scm")

;; --- An example ---

(define automa 
  (create-automaton
    '(S. D. A. A1. B.)  ; states
    '(a b c aa $)    ; symbols
    '(D. B.) ; end states

    ;;; ---- precedence matrix ----
    '((a = aa)
      (a > $)
      (aa > a)
      (aa > b)
      (aa > $)
      (b = b)
      (b > $)
      (c < a)
      (c < b)
      (c > $)
      ($ < a)
      ($ < aa)
      ($ < b)
      ($ < c)
      ($ = $)
      )

    ;;; ---- delta ----
    '((S. c (D. B.))
      (D. a (A.))
      (A. aa (A1.))
      (B. b (A1.))
      (D. S. (D.))
      (A1. D. (D.))
      (A1. B. (B.))
      (B. S. (B.))
      )))

(run-it automa '(c a aa b) 'S.)

