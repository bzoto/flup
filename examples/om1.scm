
(require "../flush+.scm")


(define automa 
  (create-automaton
    '(H L)  ; states
    '(i u d $)    ; symbols
    '(H L) ; end states

    ;;; ---- precedence matrix ----
    '(
      ($ < i)
      ($ < u)
      ($ < d)
      (u = d)
      (d = u)
      (i = d)
      (i = u)
      (u < i)
      (d < i)
      (d > d)
      (u > u)
      (d > $)
      (u > $)
      ($ = $)
      )

    ;;; ---- delta ----
    '((L u (H))
      (H d (L))
      (L d (L))
      (H i (H))
      (H u (H))
      (L i (L))

      (L L (L))
      (L H (H))
      (H L (L))
      (H H (H)))

      ))

(run-it automa '(u i u d u i d u u d d) 'L)

