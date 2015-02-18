;; first example G->A
;; MPradella MMXV

#lang racket

(require "../g2a.ss"
         "../flush++.scm")

(set-rules!
  '( (E -> E + T)
     (E -> T * F)
     (E -> n)
     (E -> << E >>)
     
     (T -> T * F)
     (T -> n)
     (T -> << E >>)

     (F -> n)
     (F -> << E >>)
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
                 *initial-state*)))
