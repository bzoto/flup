;; ------------------------------
;; Exhaustive string generator
;; (c) by Matteo Pradella, MMXIII
;; ------------------------------

#lang racket

(provide
 generate-strings
 grammar-strings
 iterated-p-car
 )

(define nonterm? cons?) 

(define (grammar->hash set-grammar)
  "translate a grammar as set into an hash table"
  (let ((res (make-hash)))
    (set-for-each 
     set-grammar
     (lambda (rule)
       (let* ((nt (car rule))
	      (hs (hash-ref res nt #f)))
	 (hash-set! res nt 
		    (if hs
			(cons (caddr rule) hs)
			(list (caddr rule)))))))
    res))

(define (sform-sub sf gramm)
  "substritute all the left parts in sf with lists of right parts"
  (let ((res '()))
    (for-each (lambda (c)
		(set! res
		      (append res 
			      (list
			       (if (nonterm? c)
				   (hash-ref gramm c)
				   (list c))))))
	      sf)
    res))

(define (iterated-p-car list-of-lists)
  "iterated cartesian product"
  (define (foldl1 fun lst)
    (foldl fun (car lst) (cdr lst)))

  (foldl1 (lambda (X Y)
	    (for*/list ((x X)
			(y Y))
	      (cond
	       ((and (list? x)(list? y))
		(append y x))
	       ((list? y) 
		(append y (list x)))
	       ((list? x)
		(append (list y) x))
	       (else
		(append (list y)(list x))))))
	  list-of-lists))

(define (sf-apply sform gramm)
  "apply all possible rules to a sentential form"
  (iterated-p-car
   (sform-sub sform gramm)))

(define (terminal-sf? sf)
  (andmap (lambda (s)
	    (not (nonterm? s))) sf))

(define (bound-and-dump sforms k)
  "filter out too long sforms, and display terminals" 
  (filter (lambda (sf)
	    (cond
	     ((terminal-sf? sf)
		(display sf)(newline)
		#f)
	     ((> (length sf) k)
	      #f)
	     (else #t)))
	  sforms))

(define (generate-strings sforms gramm bound)
  (unless (null? sforms)
    (generate-strings
     (apply append
	    (map (lambda (sf)
		   (bound-and-dump (sf-apply sf gramm) bound))
		 sforms))
     gramm
     bound))) 

(define (grammar-strings gramm-as-set bound)
  (let* ((gramm (grammar->hash gramm-as-set))
	 (sforms (map list (hash-keys gramm))))
    (generate-strings sforms gramm bound)))

;; --- tests ---

(define (test)
  (define (must-be x y) 
    (unless (equal? x y)
      (error "test error:" x '--VS-- y)))

  (define G (make-hash))
  (hash-set! G (cons 'A #f) 
	     (list (list 'b (cons 'B #f) 'b) 
		   (list 'b (cons 'A #f))))
  (hash-set! G (cons 'B #f)  
	     (list (list (cons 'A #f) 'a (cons 'B #f))
		   (list 'c 'c)))

  (must-be
   (sform-sub (list 'a (cons 'B #f) 'b (cons 'A #f)) G)
   '((a) (((A . #f) a (B . #f)) (c c)) (b) ((b (B . #f) b) (b (A . #f)))))

  (must-be
    (sf-apply (list (cons 'B #f) 'b (cons 'A #f) 'c) G)
    '(((A . #f) a (B . #f) b b (B . #f) b c)
      (c c b b (B . #f) b c)
      ((A . #f) a (B . #f) b b (A . #f) c)
      (c c b b (A . #f) c)))
 

  #t
  )
