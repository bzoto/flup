;; ------------------------------
;; Ex Matrix Ad Maxgrammar
;; (c) by Matteo Pradella, MMXIII
;; ------------------------------
;; Optimized? The horror, the horror!

#lang racket

(require
  "gengra.ss") ;; only for iterated-p-car

(provide
  chains->grammar
  compute-chains
  show-grammar
  def=.
  def>.
  def<.)

; --- interface ---
(define =. #f)
(define >. #f)
(define <. #f)

(define (def=. t) (set! =. t))
(define (def>. t) (set! >. t))
(define (def<. t) (set! <. t))
; --- end interface ---

; --- step 1, basic chains ---
(define (new-chains chains)
  (let ((res
          (apply append
                 (map iterated-p-car (map next=. chains)))))
    res))

(define (next=. chain)
  (append (list chain) 
          (list
            (set->list (=. (last chain))))))

(define (chaining chains)
  (let* ((nc  (new-chains chains))
         (ncs (list->set nc))
         (ocs (list->set chains)))
    (if (set-empty? (set-subtract ncs ocs))
      chains
      (chaining (set->list (set-union ocs ncs))))))

(define (compute-chains alphabet)
  (chaining (map list alphabet)))

; --- steps 2... ---

(define (set<. t leftset)
  (andmap identity 
          (set-map leftset 
                   (lambda (x) (set-member? (<. t) x)))))

(define (set>. rightset t)
  (andmap identity
          (set-map rightset 
                   (lambda (x) (set-member? (>. x) t)))))

(define (between x nonterm y) ; nonterm is a cons of two sets
  (let ((left  (car nonterm))
        (right (cdr nonterm)))
    (and (set<. x left)
         (set>. right y))))

(define (chain-compatible-nt chain N)
  "returns a chain decorated with lists of compatible nonterminals in N"
  (let ((res '()))
    (let loop ((c chain)
               (lchar '$))

      (set! res (append 
                  res
                  (if (eq? lchar '$)
                    '()
                    (list (list lchar)))
                  (list (cons '$
                              (cond 
                                [(eq? '$ lchar) ; left end
                                 (filter (lambda (nt) 
                                           (set>. (cdr nt) (car c))) N)]
                                [(null? c) ; right end
                                 (filter (lambda (nt) 
                                           (set<. lchar (car nt))) N)]
                                [else ; general case
                                  (filter (lambda (nt)
                                            (between lchar nt (car c))) N)])))
                  ))
      (if (null? c) 
        res
        (loop (cdr c)(car c))))))

(define (chain->right chain nonterms)
  "returns a list of right parts with inserted nonterminals"
  (map (lambda (L)
         (filter (lambda (x) (not (eq? x '$))) L))
       (iterated-p-car 
         (chain-compatible-nt chain nonterms))))


(define (chain->left chain)
  "compute the nonterminal of a chain"
  (let ((fst (car  chain))
        (lst (last chain)))

    (cons
      (if (cons? fst) ; it is a nonterminal
        (set-add (car fst) 
                 (cadr chain))
        (set fst))
      (if (cons? lst)
        (set-add (cdr lst)
                 (cadr (reverse chain)))
        (set lst)))))

(define (chains->rules chains N)
  (let ((rules
          (apply append
                 (map (lambda (c)
                        (let ((ctr (chain->right c N)))
                          (map (lambda (c) 
                                 (list (chain->left c) '-> c)) ctr)))
                      chains))))
    (values (apply set rules) ; set or rules
            (apply set (map (lambda (x) (car x)) rules))))) ; set of nonterminals

;; --- main loop ---

(define (show-nt N)
  (set-for-each N 
                (lambda (rule)
                  (display rule)(newline)
                  )))


(define (show-grammar P)
  (set-for-each P 
                (lambda (rule)
                  (let ((left  (car rule))
                        (right (caddr rule)))
                    (display left)
                    (display " -> ")
                    (for-each (lambda (x) (display x)(display " ")) right)
                    (newline)))))

(define (display-nt nt)
  (define (blorz the-set)
    (reverse 
      (cdr 
        (reverse 
          (foldr append '()
                 (map (lambda (x) (list x #\,))
                      (set->list the-set)))))))
  (let ((ls (blorz (car nt)))
        (rs (blorz (cdr nt))))
    (display "\\langle \\{")
    (for-each (lambda (s) (display s)) ls)
    (display "\\}, \\{")
    (for-each (lambda (s) (display s)) rs)
    (display "\\} \\rangle")))

(define (show-grammar-tex P)
  (set-for-each P 
                (lambda (rule)
                  (let ((left  (car rule))
                        (right (caddr rule)))
                    (display-nt left)
                    (display " & \\to & ")
                    (for-each (lambda (x) 
                                ;(display "\\ ")
                                (if (cons? x) 
                                  (display-nt x)
                                  (display x))
                                (display "\\ ")) right)
                    (display "\\\\")
                    (newline)))))



(define (step-next chains N1 P1 nt-filter) 
  (let-values ([(P2 N2) (chains->rules chains (set->list N1))])
              (let* ((newN2 (nt-filter N2))
                     (newP2 (filter-rules P2 newN2)))

                (display "----NONTERMS:----")(newline)
                (show-nt (set-subtract newN2 N1))
                (display "----GRAMMAR:-----")(newline)
                (show-grammar (set-subtract newP2 P1))
                (display "------next-------")(newline)

                ((if (set-empty? (set-subtract newN2 N1))
                   values 
                   step-next) 

                 chains
                 (set-union N1 newN2)
                 (set-union P1 newP2)
                 nt-filter))))

(define (filter-rules rules nonterms)
  (let ((new-rules (set)))
    (set-for-each rules 
                  (lambda (r)
                    (when (set-member? nonterms (car r))
                      (set! new-rules (set-add new-rules r)))))
    new-rules))

(define (chains->grammar chains nt-filter)
  (let-values ([(chains N P fl<)
                (step-next chains (set) (set) nt-filter)])
              (show-grammar-tex P)

              (display "The resulting grammar has ")
              (display (set-count P))
              (display " productions and ")
              (display (set-count N))
              (display " nonterminals.")
              (newline)
              (newline)

              P
              ))



;; ---- test cases ----

(define (test)
  (define (must-be x y) 
    (unless (equal? x y)
      (error "test error:" x '--VS-- y)))

  (def=. (lambda (x)
           (case x
             ('b (set 'a))
             (else (set)))))
  (def>. (lambda (x)
           (case x
             ('a (set 'a))
             (else (set)))))
  (def<. (lambda (x)
           (case x
             ('b (set 'b))
             (else (set)))))

  ;; --- between ---
  (must-be
    (between 'a (cons (set 'a)(set 'a 'b)) 'b)
    #f)
  (must-be 
    (between 'b (cons (set 'b)(set 'a)) 'a)
    #t)

  ;; --- chain-compatible-nt ---
  (must-be
    (chain-compatible-nt '(b a) 
                         (list (cons (set 'b)(set 'a))
                               (cons (set 'a)(set 'a))
                               (cons (set 'b)(set 'b))
                               (cons (set 'b)(set 'b 'a))))
    (list '($) '(b) (list '$ (cons (set 'b) (set 'a))) '(a) '($))
    )

  ;; --- chain->right --- 
  (must-be
    (chain->right '(b a) (list (cons (set 'b)(set 'a))
                               (cons (set 'a)(set 'a))
                               (cons (set 'b)(set 'b))
                               (cons (set 'a)(set 'b 'a))))
    (list '(b a) 
          (list 'b (cons (set 'b) (set 'a)) 'a))
    )


  ;; --- chain->left ---
  (must-be
    (chain->left (list (cons (set 'b) (set 'a)) 'a 'b 'b (cons (set 'b) (set 'a))))
    (cons (set 'b 'a) (set 'b 'a)))


  ;; --- chains->rules ---
  (let-values ([(rules nonterminals)
                (chains->rules (list '(a) '(a b)) 
                               (list (cons (set 'b)(set 'a))
                                     (cons (set 'a)(set 'b 'a))))])
              (must-be (cons rules nonterminals)
                       (cons
                         (set
                           (list (cons (set 'b 'a) (set 'a)) '-> (list (cons (set 'b) (set 'a)) 'a))
                           (list
                             (cons (set 'b 'a) (set 'b 'a))
                             '->
                             (list (cons (set 'b) (set 'a)) 'a 'b (cons (set 'b) (set 'a))))
                           (list (cons (set 'a) (set 'b 'a)) '-> (list 'a 'b (cons (set 'b) (set 'a))))
                           (list (cons (set 'b 'a) (set 'b)) '-> (list (cons (set 'b) (set 'a)) 'a 'b))
                           (list (cons (set 'a) (set 'a)) '-> '(a))
                           (list (cons (set 'a) (set 'b)) '-> '(a b)))
                         (set
                           (cons (set 'a) (set 'b))
                           (cons (set 'a) (set 'a))
                           (cons (set 'a) (set 'b 'a))
                           (cons (set 'b 'a) (set 'b))
                           (cons (set 'b 'a) (set 'a))
                           (cons (set 'b 'a) (set 'b 'a))))))

  #t
  )
