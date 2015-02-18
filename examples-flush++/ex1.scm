;; MPradella, 2010.04.23

#lang racket

(require "../flush++.scm")

;; --- An example ---

(define automa 
  (create-automaton
   '(S. D. B. C. E. F.)  ; states
   '(a b c d e f + $)    ; symbols
   '(C.) ; end states

    ;;; ---- precedence matrix ----
   '((a < a)
     (a = b)
     (a = d)
     (b > b)
     (b > d)
     (b > +)
     (c > e)
     (c > f)
     (c > $)
     (d > b)
     (d > d)
     (d > +)
     (e > f)
     (e > $)
     (f > e)
     (f > $)
     (+ < a)
     (+ < c)
     (+ = e)
     (+ = f)
     (+ < +)
     ($ < a)
     ($ < c)
     ($ < +)
     ($ = $)
     )

    ;;; ---- delta push & internal ----
   '((S. a (D. B.))
     (D. c (C.))
     (D. a (D.))
     (D. d (D.))
     (D. + (B. D.))
     (D. c (C.))
     (B. a (B.))
     (B. b (B.))
     (B. + (B. D.))
     (B. c (C.))
     (C. e (E.))
     (C. f (F.)))

    ;;; --- delta pop ---
   '((D. S. (D.))
     (D. B. (D.))
     (D. D. (D.))
     (B. S. (B.))
     (B. B. (B.))
     (B. D. (B.))
     (C. S. (C.))
     (C. B. (C.))
     (C. D. (C.))
     (E. D. (C.))
     (F. B. (C.))
     )

   ))

(run-it automa '(a a b b + a a a d d d + a a b b + c f e f) 'S.)

