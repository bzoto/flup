;; MPradella, 2010.04.23

;#lang scheme

(require "../flush+.scm")

;; --- An example ---

(define automa 
  (create-automaton
    '(S. D. B. C. E. F. BD.)  ; states
    '(a b c d e f + $)    ; symbols
    '(C.) ; end states

    ;;; ---- precedence matrix ----
    '((a < a)
      (a = b)
      (a = d)
      (b > b)
      (b > +)
      (c > e)
      (c > f)
      (c > $)
      (d > d)
      (d > +)
      (e > e)
      (e > f)
      (e > $)
      (f > e)
      (f > f)
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

    ;;; ---- delta ----
    '((S. a (BD.))
      (S. c (C.))
      (D. c (C.))
      (D. a (D.))
      (D. d (D.))
      (D. + (BD.))
      (B. a (B.))
      (B. b (B.))
      (B. + (BD.))
      (B. c (C.))
      (C. e (E.))
      (C. f (F.))
      (BD. b (B.))
      (BD. d (D.))
      (BD. + (BD.))
      (BD. a (BD.))
      (BD. c (C.))


      (D. S. (D.))
      (D. B. (D.))
      (D. D. (D.))
      (D. BD. (D.))
      (BD. S. (BD.))
      (BD. B. (BD.))
      (BD. D. (BD.))
      (BD. BD. (BD.))
      (B. S. (B.))
      (B. B. (B.))
      (B. D. (B.))
      (B. BD. (B.))
      (C. S. (C.))
      (C. B. (C.))
      (C. D. (C.))
      (C. BD. (C.))
      (E. D. (C.))
      (F. B. (C.))
      )))

(run-it automa '(a a b b + a a a d d d + a a b b + c f e f) 'S.)

(assert (run-it automa '(a a b b + a a d d + c e f) 'S.))
(assert (run-it automa '(a a b b + a a b b + c f f) 'S.))
(assert (not (run-it automa '(a a b b + a a b b + c e f) 'S.)))
(assert (not (run-it automa '(a a b b + a a d d d + c e f) 'S.)))
(assert (not (run-it automa '(a a a b b + a d + c e f) 'S.)))


