;; Precedence Operator Automaton, in the incarnation of SICOMP
;; variant with separated delta_internal and delta_push

;; MPradella, 2014.05.30

;; --------------------------------------------------------------------------
;;
;; Copyright (C) 2010-2014 Matteo Pradella (matteo.pradella@polimi.it)
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;;
;; --------------------------------------------------------------------------



#lang racket

(require
  (only-in srfi/1 list-index)
  srfi/25)

(provide
  assert
  run-it
  create-automaton
  show-conf
  filter-symbol
  )

(define-syntax assert
  (syntax-rules ()
    ((assert cond)
     (unless cond
       (error (format "assertion violation on ~s" (quote cond) ))))))

;; *** data structure ***

(define-struct automaton
  (states symbols end-states precedences
          delta-pop delta-push delta-internal))

(define (create-automaton states symbols end-states prec
                          d-push
                          d-pop
                          . d-int)
  (let ((auto (make-automaton
		states
		symbols
		end-states
		(make-array (shape 0 (length symbols) 0 (length symbols)) #f)
                ;; delta-pop
		(make-array (shape 0 (length states)  0 (length states))  '())
                ;; delta-push
		(make-array (shape 0 (length states)  0 (length symbols)) '())
                ;; delta-internal
		(make-array (shape 0 (length states)  0 (length symbols)) '())
		)))

    (display "states: ")(displayln (length states))
    (display "delta-pop: ")(displayln (length d-pop))
    (display "delta-push: ")(displayln (length d-push))
    (display "delta-internal: ")(displayln (length d-int))

    (displayln "precs...")
    (for-each 
      (lambda (x) (apply precedence-set! (cons auto x)))
      prec)

    (displayln "d-push..")
    (for-each
     (lambda (x)
       (apply delta-set! (append (list 'push auto) x)))
     d-push)

    (displayln "d-pop..")
    (for-each
     (lambda (x)
       (apply delta-set! (append (list 'pop auto) x)))
     d-pop)

    (displayln "d-int..")
    (for-each
     (lambda (x)
       (apply delta-set! (append (list 'int auto) x)))
     (if (null? d-int)
         d-push
         (car d-int))) ;; if the delta-internal is not present, we use delta-push for it

    auto))



(define (list-position Da a)
  (let ((res (list-index (lambda (x) (equal? a x))  Da)))
    (if res res (error "bad element" a Da))))

(define (where automa x)
  (if (member x (automaton-states automa))
    (list-position (automaton-states automa) x)
    (list-position (automaton-symbols automa) x)))

(define (precedence-set! automa a prec b)
  (let ((p (automaton-precedences automa))
	(wa (where automa a))
	(wb (where automa b)))
    (if (array-ref p wa wb)
      (error "precedence already set: " a prec b)
      (array-set! (automaton-precedences automa) 
		  (where automa a) 
		  (where automa b) prec))))

(define (delta-set! what automa a b next)
  (array-set!
    (case what 
      ('pop   (automaton-delta-pop automa)) 
      ('push  (automaton-delta-push automa)) 
      ('int   (automaton-delta-internal automa)))
    (where automa a) 
    (where automa b) next))





;; accessors
(define (<=> automa a b)
  (array-ref (automaton-precedences automa) 
	     (where automa a)
	     (where automa b)))

(define (delta-push automa a b)
  (with-handlers ([exn:fail? (lambda (exn) '())])
    (array-ref (automaton-delta-push automa) 
               (where automa a)
               (where automa b))))
(define (delta-pop automa a b)
  (with-handlers ([exn:fail? (lambda (exn) '())])
    (array-ref (automaton-delta-pop automa) 
               (where automa a)
               (where automa b))))

(define (delta-internal automa a b)
  (with-handlers ([exn:fail? (lambda (exn) '())])
    (array-ref (automaton-delta-internal automa) 
               (where automa a)
               (where automa b))))

;; ---- Configuration:
;; (stack input state)
;; stack = (S0 S1 S2 ...) S0 = top
;; each Si is a vector #(terminal state)
;; input = (i0 i1 i2 ...)


(define (the-top input) 
  "Takes the top of the stack or the next input character"
  (if (cons? input) (car input) '()))

(define (lstate stack)
  "last state, i.e. state of the top"
  (cadr (car stack)))

(define (lsymbol stack)
  "last symbol, i.e. terminal of the top"
  (car (car stack)))

(define (state-of config)
  (caddr config))

(define (filter-symbol s)
  (cond
    [(eq? s '$)  "#"]
    [(eq? s '>>) "}"]
    [(eq? s '<<) "{"]
    [else s]))

(define (show-stack-item it)
  (let ((term (car it))
	(state (cadr it)))
    (display "[")
    (display (filter-symbol term))
    (display " ")
    (display state)
    (display "]")))

(define (show-stack st)
  (for-each show-stack-item st))

(define (show-input input)
  (for-each
    (lambda (term)
      (display (filter-symbol term)))
    input))

(define (show-conf conf prec)
  "displays configuration conf"
  (display (state-of conf))
  (display "  ")
  (show-stack (reverse (car conf)))
  (display "  ")
  (display prec)
  (display "  ")
  (show-input (cadr conf))
  (newline))


(define (next-configuration automa config)
  (let* ((stack      (car config))
	 (top        (car stack))
	 (top-symbol (lsymbol stack))
	 (top-state  (lstate  stack))
	 (the-string (cadr config))
	 (input      (the-top the-string))
         (the-state  (state-of config))
	 (prec       (<=> automa top-symbol input)))

    (show-conf config prec)

    (cond
      [(not prec) '()]

      ;; --- acceptance condition ---
      [(and (eq? top-symbol '$)
	    (eq? input '$)
	    (member the-state (automaton-end-states automa)))
       #t]

      ;; --- POP ---
      [(eq? prec '>) 
       (let* ((next-state (delta-pop automa  
                                     the-state
                                     top-state)))
         (map (lambda (ns)
                (list
                 ;; stack
                 (cdr stack)
                 ;; input
                 the-string
                 ;; state
                 ns))
              next-state)

         )]

      ;; --- PUSH ---
      [(eq? prec '<) 
       (let ((next-state (delta-push automa 
                                     the-state 
                                     input)))
         (map (lambda (ns)
                (list
                 ;; stack
                 (cons (list 
                        input 
                        the-state) 
                       stack)
                 ;; input
                 (cdr the-string)
                 ;; state
                 ns))
              next-state)
         )]
      ;; --- INTERNAL ---
      [else
	(let ((next-state (delta-internal automa
				      the-state 
				      input)))
	  (map (lambda (ns)
		 (list
		   ;; stack
		   (cons (list 
                          input 
                          top-state) 
			 (cdr stack))
		   ;; input
		   (cdr the-string)
                   ;; state
                   ns))
	       next-state)
	  )]
      )))


(define (run-it automa input axiom)
  (for-each (lambda (x) (unless 
			  (member x (automaton-symbols automa))
			  (error "bad symbol: " x))) 
	    input)

  (if (list? axiom)
    ;; multiple initial states
    (run-it-helper automa
		   (map (lambda (ax)
			  (list 
			    ; stack
			    (list (list '$ '-)) 
			    ; input 
			    (append input '($))
                            ; state
                            ax))
			axiom))

    ;; single initial state
    (run-it-helper automa
		   (list 
		     (list 
		       ; stack
		       (list (list '$ '-)) 
		       ; input 
		       (append input '($))
                       ; state
                       axiom)))))

(define (run-it-helper automa c0s)
  (if (null? c0s)
    (begin
      (display "-----> No\n\n")
      #f)
    (let ((c1s (next-configuration automa (car c0s))))
      (when (null? c1s)
	(display "---Backtrack---")(newline))
      (if (eq? c1s #t)
	(begin
	  (display "-----> Yes\n\n")
	  #t)
	(run-it-helper automa (append c1s (cdr c0s)))))))


