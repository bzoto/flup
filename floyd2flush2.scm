;; Floyd Grammar -> Precedence Automata translator
;; MPradella, MMX
;; horribly procedural with lots of globals, but works...

;; --------------------------------------------------------------------------
;;
;; Copyright (C) 2010 Matteo Pradella (pradella@elet.polimi.it)
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



#lang scheme

(require 
  scheme/set
  "flush+.scm"
  )

(provide
  set-rules!
  set-terminals!
  set-nonterminals!
  set-axiom!
  toggle-debug!

  compute-initial-states
  compute-delta
  the-states
  the-terminals
  end-states
  )


(define *debug* #f)
(define (toggle-debug!)
  (set! *debug* (not *debug*)))


(define-syntax ==> ; logical implication
  (syntax-rules ()
		((_ x y) (or (not x) y))))

(define-syntax <==> 
  (syntax-rules ()
		((_ x y) (and 
			   (or (not x) y)
			   (or x (not y))))))

;; accessors 

(define (lhs rule)
  (car rule))

(define (rhs rule)
  (cddr rule))

(define (rid rule)
  (cadr rule))

(define (compute-initial-states)
  (for/list ((i (in-range 1 (add1 (hash-ref max-rid *axiom*)))))
	    (cons (cons *axiom* i) '_)))


;; data structures


;; relations and their definitions

(define (related? x rel y)
  (set-member? (hash-ref rel x (set)) y))

(define max-rid (make-hash))
(define (compute-max-rid)
  (for-each
    (lambda (r)
      (let* ((nt  (lhs r))
	     (id  (rid r))
	     (res (hash-ref max-rid nt 0)))

	(when (> id res)
	  (hash-set! max-rid nt id))))
    *rules*))


(define (get-rule nt id)
  (call/ec (lambda (kk)
	     (for ((r *rules*))
		  (when (and (equal? (lhs r) nt)
			     (= id (rid r)))
		    (kk r))))))

(define (starts-with-nt? rule)
  (let ((r (car (rhs rule))))
    (if (member r *nt*) r #f)))

(define FR (make-hash))
(define (compute-FR)
  (let ((NT (apply set *nt*)))

    (for-each (lambda (r)
		(let* ((fln (starts-with-nt? r))
		       (gnt (cons (lhs r) (rid r)))
		       (maxr (if fln (hash-ref max-rid fln) 0))
		       (refl  (set gnt))) ; {gnt} if reflexive
		  (hash-set! FR gnt
			     (if fln
			       (let loop ((i 1)(s refl)) 
				 (if (<= i maxr)
				   (loop (add1 i)(set-add s (cons fln i)))
				   s))
			       refl) 
			     )))
	      *rules*)

    ; compute transitive closure 
    (for ((i (in-range 0 (length *nt*))))
	 (for-each
	   (lambda (n)
	     (set-for-each
	       (hash-ref FR n)
	       (lambda (x)
		 (hash-set! FR n (set-union
				   (hash-ref FR x) 
				   (hash-ref FR n))))))
	   *ntx*))))

(define (nt-before-t? rule nt)
  (let loop ((r (rhs rule)))
    (if (not (null? r))
      (let ((x (car r))
	    (y (cdr r)))
	(if (and (member x *tr*)
		 (not (null? y))
		 (equal? (car y) nt))
	  #t
	  (loop y)))
      #f)))

(define (starts-with-t? rule)
  (member (car (rhs rule)) *tr*))

(define HR (make-hash))
(define (compute-HR)
  (for* ((A *ntx*)
	 (B *ntx*)
	 (C *ntx*))

	(let ((oldv (hash-ref HR A (set)))
	      (rA (get-rule (car A) (cdr A)))
	      (rB (get-rule (car B) (cdr B))))

	  (when (and (nt-before-t? rA (car C))
		     (set-member? (hash-ref FR C) B)
		     (starts-with-t? rB))

	    (hash-set! HR A (set-add oldv B)))))

  (for* ((A *ntx*)
	 (i (in-range 1 (add1 (hash-ref max-rid *axiom*))))
	 #:when 
	 (related? (cons *axiom* i) FR A))

	(let ((oldv (hash-ref HR (cons *axiom* i) (set))))
	  (hash-set! HR (cons *axiom* i) (set-add oldv A)))))


(define KR (make-hash))
(define (compute-KR)
  (for* ((A *ntx*)
	 (B *ntx*)
	 (C *ntx*))

	(let ((oldv (hash-ref KR A (set)))
	      (rA (get-rule (car A) (cdr A)))
	      (rB (get-rule (car B) (cdr B))))

	  (when (and (nt-before-t? rA (car C))
		     (set-member? (hash-ref FR C) B))

	    (hash-set! KR A (set-add oldv B)))))

  (for* ((A *ntx*)
	 (i (in-range 1 (add1 (hash-ref max-rid *axiom*))))
	 #:when 
	 (related? (cons *axiom* i) FR A))

	(let ((oldv (hash-ref KR (cons *axiom* i) (set))))
	  (hash-set! KR (cons *axiom* i) (set-add oldv A)))))



(define (fill-relations)
  (compute-max-rid)
  (compute-FR)
  (when *debug* 
    (newline)(display "F: ")(write FR)(newline))
  (compute-KR)
  (when *debug* 
    (newline)(display "K: ")(write KR)(newline))
  (compute-HR)
  (when *debug* 
    (newline)(display "H: ")(write HR)(newline)(newline))
  )


(define *rules* #f)

(define (set-rules! rules)
  (set! *rules* rules))

(define *tr* #f)

(define (set-terminals! terms)
  (set! *tr* terms))

(define *axiom* #f)
(define (set-axiom! axiom)
  (set! *axiom* axiom))

(define *nt* #f)
(define *ntu* #f)
(define *ntx* #f)
(define *ntxu* #f)
(define *states* #f)

(define (set-nonterminals! nt)
  (set! *nt* nt)
  (set! *ntu* (cons '_ *nt*))
  (set! *ntx* 
    (map 
      (lambda (r)
	(cons (lhs r) (rid r)))
      *rules*))
  (set! *ntxu* (cons '_ *ntx*))
  (set! *states* (for*/list ((x *ntx*)
			     (y *ntxu*))
			    (cons x y)))
  )

(define (the-states) *states*)
(define (the-terminals) *tr*)


;; --------
;; the code
;; --------

(define (find-all-t-t rule)
  (let loop ((rr (rhs rule)) (out '()))
    (if (null? rr) 
      out
      (loop (cdr rr)
	    (if (and (member (car rr) *tr*)
		     (not (null? (cdr rr)))
		     (member (cadr rr) *tr*))
	      (cons (cadr rr) out)
	      out)))))

(define (find-all-t-nt-t rule)
  (let loop ((rr (rhs rule)) (out '()))
    (if (null? rr) 
      out
      (loop (cdr rr)
	    (if (and (member (car rr) *tr*)
		     (not (null? (cdr rr)))
		     (member (cadr rr) *nt*)
		     (not (null? (cddr rr)))
		     (member (caddr rr) *tr*))
	      (cons (cons (cadr rr) (caddr rr)) out)
	      out)))))

(define (starts-nt-t? rule)
  (let ((rr (rhs rule)))
    (if (> (length rr) 1)
      (if (and
	    (member (car rr) *nt*)
	    (member (cadr rr) *tr*))
	(cons (car rr)(cadr rr))
	#f)
      #f)))

(define (starts-t? rule)
  (let ((rr (rhs rule)))
    (if (null? rr)
      #f
      (if (member (car rr) *tr*)
	(car rr)
	#f))))

(define (starts-nt? rule)
  (let ((rr (rhs rule)))
    (if (null? rr)
      #f
      (if (member (car rr) *nt*)
	(car rr)
	#f))))

(define (ends-nt? rule)
  (let ((res (member (last rule) *nt*)))
    (if res
      (car res)
      #f)))

(define (ends-t? rule)
  (let ((res (member (last rule) *tr*)))
    (if res
      (car res)
      #f)))


(define (compute-delta)

  (define *dp+* '())

  (fill-relations)
  (dump-grammar)

  (for-each
    (lambda (r) ; r : rule
      (let ((c1 (find-all-t-t r))
	    (c2 (starts-t? r))
	    (c3 (find-all-t-nt-t r))
	    (c4 (starts-nt-t? r))
	    (c5 (ends-nt? r))
	    (c6 (ends-t? r))
	    (Yi (cons (lhs r)(rid r))))

	(when *debug* (display "rule: ")(display r)(newline))

	;; conditions for push moves
	(when *debug* (display "*****C1")(newline))
	(when (not (null? c1))
	  (set! *dp+* 
	    (append *dp+*
		    (dump-dp
		      (map (lambda (x)
			     (list (cons Yi '_) x (list
						    (cons Yi
							  (if (equal? x (last (rhs r)))
							    Yi
							    '_)))))
			   c1)))))

	(when *debug* (display "*****C2")(newline))
	(when c2
	  (set! *dp+*
	    (append *dp+*
		    (dump-dp
		      (for/list ((Z *ntx*)
				 #:when
				 (related? Z HR Yi))
				(list (cons Z '_) c2 (list 
						       (cons Yi
							     (if (equal? c2 (last (rhs r)))
							       Yi
							       '_)))))))))
	(when *debug* (display "*****C3")(newline))
	(when (not (null? c3))
	  (for-each 
	    (lambda (lst)
	      (set! *dp+* (append *dp+* (dump-dp lst))))

	    (map (lambda (Xx)
		   (let ((X (car Xx))
			 (x (cdr Xx)))
		     (for/list ((k (in-range 1 (add1 (hash-ref max-rid X)))))
			       (list (cons Yi (cons X k)) x (list
							      (cons Yi
								    (if (equal? x (last (rhs r)))
								      Yi
								      '_)))))))
		 c3)))

	(when *debug* (display "*****C4")(newline))
	(when c4
	  (for-each 
	    (lambda (lst)
	      (set! *dp+* (append *dp+* (dump-dp lst))))

	    (let ((X (car c4))
		  (x (cdr c4)))
	      (for/list ((Z *ntx*)
			 #:when
			 (related? Z KR Yi)) 

			(for/list ((k (in-range 1 (add1 (hash-ref max-rid X))))) 
				  (list (cons Z (cons X k)) x (list 
								(cons Yi
								      (if (equal? c2 (last (rhs r)))
									Yi
									'_)))))))))


	;; --- flush stuff ---
	(when *debug* (display "*****C5")(newline))
	(when c5
	  (for-each 
	    (lambda (lst)
	      (set! *dp+* (append *dp+* (dump-dp lst))))

	    (let ((X c5)
		  (nt1 (starts-nt? r)))
	      (for*/list ((Z *ntx*)
			  (W *ntx*)
			  #:when
			  (and (related? Z KR Yi)
			       (==> nt1 (equal? nt1 (car W)))))
			 (for/list ((k (in-range 1 (add1 (hash-ref max-rid X))))) 
				   (list (cons Yi (cons X k)) 
					 (cons Z (if nt1 W '_)) 
					 (list (cons Z Yi))))))))


	(when *debug* (display "*****C6")(newline))
	(when c6
	  (set! *dp+*
	    (append *dp+*
		    (dump-dp
		      (let ((nt1 (starts-nt? r)))
			(for*/list ((Z *ntx*)
				    (W *ntx*)
				    #:when
				    (and (related? Z KR Yi)
					 (==> nt1 (equal? nt1 (car W)))))
				   (list (cons Yi Yi) 
					 (cons Z (if nt1 W '_)) 
					 (list (cons Z Yi)))))))))

	))
    *rules*)

  *dp+*
  )


(define (end-states axiom)
  (append
    (for*/list ((p *ntx*)
		(q *ntx*)
		(r *rules*)
		#:when
		(and
		  (eq? (car p) axiom)
		  (eq? (lhs r) axiom)
		  (and (eq? (car (rhs r)) (car q))
		       (null? (cdr (rhs r))))))
	       (cons p q))

    ; Not needed, if in Fischer Normal Form
    ;     (for*/list ((p *ntx*)
    ; 		(q *ntx*)
    ; 		#:when
    ; 		(and
    ; 		  (eq? (car p) axiom)
    ; 		  (eq? (car q) axiom)))
    ; 	       (cons p q))

    ))

(define (dump-dp a)
  (when *debug*
    (newline)
    (for-each 
      (lambda (x)
	(display (car x))
	(display "\t ")
	(display (cadr x))
	(display "\t ---> ")
	(display (caddr x))
	(newline))

      a)
    (newline))
  a)


(define (dump-grammar)
  (display "Grammar:")(newline)
  (for-each (lambda (r)
	      (display (lhs r))
	      (display " -> ")
	      (for-each (lambda (s) (display (filter-symbol s))(display " ")) (rhs r))
	      (newline))
	    *rules*)
  (newline))

