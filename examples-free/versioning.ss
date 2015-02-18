;; Maxgrammar that produces the traces of execution of a system for version management of files:
;; rollbacks to previous versions are admitted, but there cannot be uncommitted write operations. 
;;
;; The axiom of the free grammar has only renaming rules: its rhs are all nonterminals whose left set contains 'op && whose right set contains 'cl.
;;
;; Disclaimer: The execution of this file is slow.
;; -----------------

#lang racket


(require "../maxgr.ss" 
	 "../gengra.ss")


(def=. (lambda (x)
	 (case x
	   	('op (set 'cl))	
	   	('sv (set 'rb))	
	   	('wr (set 'ud))
	   	(else (set)))))

(def>. (lambda (x)
	 (case x
	   ('sv (set 'cl))	
	   ('rb (set 'rb 'cl))
	   ('wr (set 'sv 'rb))
	   ('ud (set 'sv 'rb 'ud 'cl))
	   (else (set)))))

(def<. (lambda (x)
	 (case x
	   ('op (set 'sv 'rb 'wr 'ud))		 	
	   ('sv (set 'sv 'wr 'ud))
	   ('rb (set 'sv 'wr 'ud))
	   ('wr (set 'wr))
	   ('ud (set 'wr))
	   (else (set)))))

(grammar-strings
(chains->grammar (compute-chains '(op cl sv rb wr ud)) identity)
4)
