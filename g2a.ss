;; OPG to OPA - new, SICOMP variant
;; MPradella MMXIV

#lang racket

(require
  (only-in srfi/1 delete-duplicates!)
 "./flush++.scm"
 "./precs.scm")

(provide
  set-rules!
  set-terminals!
  set-nonterminals!
  set-axioms!

  make-automaton
  get-initial-states
  )

(define *rules* #f)
(define *terminals* #f)
(define *nonterminals* #f)
(define *NT* #f)
(define *axioms* #f)

(define (set-rules! r)
  (set! *rules* r))
(define (set-terminals! r)
  (set! *terminals* r))
(define (set-nonterminals! r)
  (set! *nonterminals* r)
  (set! *NT* (map list r)))
(define (set-axioms! x)
  (set! *axioms* x))

(define (get-initial-states)
  (list (cons '() '())))

(define *Q* '())
(define *P* '())


;; a rule is like this: (A -> B b C)
(define get-left  car)
(define get-right cddr)


(define (find-left right-part)
  (map list
       (map get-left
            (filter (lambda (x)
                      (equal? (get-right x) right-part))
                    *rules*)))) 

;; a right part of a rule is a list
;; hence we compute the prefixes of a right part
(define (prefixes lst)
  (let loop ((result (list
                      (list (car lst))))
             (current (cdr lst)))
    (if (null? current)
        result
        (loop (cons
               (append (car result)
                       (list (car current)))
               result)
              (cdr current)))))


(define (Q-set!)
  (set! *P*  ;; right parts U epsilon
        (delete-duplicates!
         (cons '()
               (apply append
                      (map prefixes
                           (map get-right *rules*))))))
  (set! *Q*
        (delete-duplicates!
         (append *NT* *P*))))

(define (the-states)
  (for*/list ((q1 *Q*)
              (q2 *P*))
    (cons q1 q2)))

(define (end-states)
  (map list (map list *axioms*)))

(define (delta-internal)
  (for*/list ((a *terminals*)
              (alp1 *Q*)
              (alp2 *P*))

    (list (cons alp1 alp2) a
          (list 
           (if (not (member alp1 *NT*))
               (cons (append alp1 (list a))
                     alp2) 
               (cons (append alp2 alp1 (list a))
                     alp2))))))
                                        
(define (delta-push)
  (for*/list ((a *terminals*)
              (alp1 *Q*)
              (alp2 *P*))

    (list (cons alp1 alp2) a
          (list 
           (if (not (member alp1 *NT*))
               (cons (list a)
                     alp2) 
               (cons (append alp1 (list a))
                     alp2))))))
  

(define (delta-pop)
  (for*/list ((alp1 *Q*)
              (alp2 *P*)
              (bet1 *Q*)
              (bet2 *P*))

    (let ((gamma (if (not (member bet1 *NT*))
                     bet1
                     bet2)))

      (list (cons alp1 alp2)
            (cons bet1 bet2)
            (map 
             (lambda (x)
                (cons x gamma))
             
             (if (not (member alp1 *NT*))
                 (find-left alp1)
                 (find-left (append alp2 alp1))))))))


(define (make-automaton)
  (Q-set!)

  (create-automaton
   (the-states)
   (cons '$ *terminals*)
   (end-states)

   ;; OP MATRIX
   (delete-duplicates!
    (apply append
           (map
            (lambda (nt)
              (compute-precs *rules* *nonterminals* *terminals* nt))
            *axioms*)))

   ;; Delta
   (delta-push)
   (delta-pop)
   (delta-internal)
   ))
