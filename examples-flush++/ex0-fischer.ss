;; first example G->A
;; MPradella MMXV

#lang racket

(require "../g2a.ss"
         "../flush++.scm")

(set-rules!
   '( (E -> E + T)
      (E -> E + F)
      (E -> T + T)
      (E -> F + F)
      (E -> F + T)
      (E -> T + F)
 
      (T -> T * F)
      (T -> F * F)
 
      (F -> n)
      (F -> << E >>)
      (F -> << T >>)
      ))
(set-nonterminals! '(E T F))
(set-terminals! '(+ * n  << >> ))
(set-axioms! '(E T F))


(define *automaton* (make-automaton))
(define *initial-state* (get-initial-states))

(assert
 (run-it *automaton*
         '(n + n * << n + n >>) 
         *initial-state*))

(assert (not
         (run-it *automaton*
                 '(n + n * << + n >>) 
                 *initial-state*)
         ))
