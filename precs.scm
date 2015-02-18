;; MPradella, end of April MMX
;; compute precedences

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

(require scheme/set)

(provide 
  compute-precs)



(define (lhs rule)
  (car rule))

(define (rhs rule)
  (cddr rule))


;; --- globals ---

(define *rules* #f)
(define *nt* #f)
(define *tr* #f)
(define *prec+* (set (list '$ '= '$)))

;; --- utils ---

(define (tclosure what) ;; what = car for Left, = last for Right
  (define the-left (make-hash))
  (define (step)
    (for-each (lambda (nt)
		(let* ((ls (hash-ref the-left nt)))
		  (set-for-each
		    ls
		    (lambda (s)
		      (hash-set! the-left nt
				 (set-union (hash-ref the-left s)
					    (hash-ref the-left nt))) ))))
	      *nt*))

  (for-each (lambda (s)
	      (hash-set! the-left s (set)))
	    (append *nt* *tr*))

  (for-each (lambda (r)
	      (let* ((l (lhs r))
		     (rr (rhs r))
		     (ls (hash-ref the-left l))
		     (item (what rr)))
		(hash-set! the-left l (set-add ls item))
		(when (and (> (length rr) 1) ;; !!!!
			   (member item *nt*))
		  (hash-set! the-left l 
			     (set-add (hash-ref the-left l) 
				      (if (equal? what car)
					(cadr rr)
					(cadr (reverse rr))))))))
	    *rules*)

  (for ((i (in-range 0 (length *nt*))))
       (step))

  (let ((terms (apply set *tr*)))
    (for-each
      (lambda (x)
	(let ((v (hash-ref the-left x)))
	  (hash-set! the-left x (set-intersect terms v)))) 
      *nt*))

  the-left)



(define (find2 rrhs)
  (if (> (length rrhs) 1)
    (let* ((s1 (car rrhs))
	   (s2 (cadr rrhs)))
	(list s1 s2))
    #f))

(define (find-all-2 rrhs out)
  (if (> (length rrhs) 1)
    (find-all-2 (cdr rrhs) (if (find2 rrhs) 
			     (cons (find2 rrhs) out) out))
    out))

(define (rel>< x y)
  (cond 
    [(cons? x)
    (set-for-each 
      (cdr x)
      (lambda (a)
	(set! *prec+* (set-add *prec+* (list a '> y)))))]
    [(cons? y)
    (set-for-each
      (car y)
      (lambda (a)
	(set! *prec+* (set-add *prec+* (list x '< a)))))]
    [else #f]))


;; --- the main stuff ---

(define (compute-precs rules nt tr axiom)
  (set! *rules* rules)
  (set! *nt* nt)
  (set! *tr* tr)

  (let* ((LL (tclosure car))
	 (RR (tclosure last))

	 (modrules
	   (map 
	     (lambda (r)
	       (map (lambda (s)
		      (if (member s *nt*)
			(cons (hash-ref LL s)
			      (hash-ref RR s))
			s))
		    (rhs r)))
	     *rules*))

	 (termrules
	   (map
	     (lambda (r)
	       (filter (lambda (x) (member x *tr*))
		       r))

	     modrules)))

    (set! termrules
      (foldr append '()
	     (map (lambda (x) (find-all-2 x '())) termrules)))

    (for-each 
      (lambda (x) (set! *prec+* 
		    (set-add *prec+* (list (car x) '= (last x))))) 
      termrules)

    (set! modrules
      (foldr append '()
	     (map (lambda (x) (find-all-2 x '())) modrules)))

    (for-each 
      (lambda (r)
	(apply rel>< r))
      modrules)

    ;; borders
    (set-for-each
      (hash-ref LL axiom)
      (lambda (x)
	(set! *prec+* (set-add *prec+* 
			       (list '$ '< x)))))
    (set-for-each
      (hash-ref RR axiom)
      (lambda (x)
	(set! *prec+* (set-add *prec+* 
			       (list x '> '$)))))

    ;; translate the set *prec+* into a list
    (let ((out '())) 
      (set-for-each
	*prec+*
	(lambda (x) (set! out (cons x out))))
      out)
    ))

