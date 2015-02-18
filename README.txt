----------------------------------------------------
FLUP, a tool for Ooperator Precedence Automata (OPA)
 (c) 2010-2014 by Matteo Pradella
----------------------------------------------------

First of all, CAVEAT EMPTOR: Flup is a very simple and unstable
*research prototype*. 

Flup contains:
1) an interpreter of non-deterministic OPA (file flush+.scm)
2) a OP Grammar to OP Automata translator (file floyd2flush2.scm)
3) an new interpreter of non-deterministic OPA, in the variant that does not push with equal in precedence (file flush++.scm)
4) a new algorithm for OP Grammar to deterministic OPA translation (file g2a.ss)

The model and algorithms are as described in the paper:
V. Lonati, D. Mandrioli, M. Pradella, Precedence Automata and Languages, The
6th International Computer Science Symposium in Russia (CSR), LNCS 6651, pp.
291-304, June 14-18, 2011

And in the submitted:
V. Lonati, D. Mandrioli, F. Panella, M. Pradella,
Operator Precedence Languages: Their Automata-theoretic and Logic Characterization

Flup is written in the Racket dialect of Scheme, freely available at:
http://racket-lang.org/

-- Max-Grammars and Free Languages:

There is also a tool for generating a max-grammar from a given precedence
matrix.  Its source code is located in maxgr.ss, used in various examples of
free grammars contained in the folder "examples-free". The tool contains also
an exhaustive string generator (it stops at a given length), "gengra.ss".

An illustrated example is given below (2).



--- EXAMPLES AND BASIC USAGE (1) ---

Many examples are included in the examples directories. To run, e.g., ex1, the
user has to type at the shell prompt the following command:

racket --script examples/ex1.scm

The output presents the actual computation performed by the automaton, possibly
with backtracking paths. Each step contains e.g. the following information:

[# B][+' D][+' B][a' B][a' B][b B]    >    b+cfef#

The current configuration of the stack is presented on the left, then there is
the found precedence (in this case b > b); last, the current part of the
input string.

Important note: in the paper the terminator symbol is #, like in the output
before. Unfortunately, this is a reserved symbol in Scheme, so we use $
instead, for input.

An input file for Flup typically contains the automaton definition,
e.g.:

(define automa 
  (create-automaton
    '(S. D. B. C. E. F.)  ; this list contains the states of the automaton
    '(a b c d e f + $)    ; input symbols, $ is the special
                          ; terminator marker usually written #
    '(C.)                 ; list of end states   

    ;;; ---- precedence matrix ----
    '((a < a)
      (a = b)
      (a = d)

	...

      ($ < c)
      ($ < +)
      ($ = $)
      )

   ;;; ---- delta ----
    '((S. a (D. B.))
      (D. c (C.))
      (D. a (D.))
      (D. d (D.))
      (D. + (B. D.))
      (D. c (C.))
      (B. a (B.))
      (B. b (B.))
      (B. + (B. D.))
      (B. c (C.))
      (C. e (E.))
      (C. f (F.))

      (D. S. (D.))
      (D. B. (D.))
      (D. D. (D.))
      (B. S. (B.))
      (B. B. (B.))
      (B. D. (B.))
      (C. S. (C.))
      (C. B. (C.))
      (C. D. (C.))
      (E. D. (C.))
      (F. B. (C.))
      )))


Then we can feed a string to the automaton with:

(run-it automa '(a a b b + a a a) 'S)

where "automa" is the name of the automaton, and in this case the string is:
aabb+aaa. The last parameter, S, is the starting symbol. 



--- THE OP GRAMMAR TO AUTOMATA TRANSLATOR ---

Caveat: the input grammar must be (almost) in Fischer Normal Form (indeed, it
may also be non-invertible).

Here is a simple example script for the translation:

First, we have to define rules. The present implementation uses a simple
intermediate format illustrated in the following example:

(define rs '((S 1 E)	       ; S -> E, first rule for S
             (E 1 E + T)       ; E -> E + T, first rule for E
             (E 2 T x a)       ; E -> T x A, second rule for E
             (E 3 a)           ; E -> a, third rule for E
             (T 1 T x a)       ; T -> T x a, first rule for T
             (T 2 a)           ; T -> a, second rule for T
             ))

Then we have to define respectively the list of nonterminals:

(define nt '(S E T))

and the list of terminals:

(define tr '(a x + $))

The following commands are used to set the main parameters.

(set-rules! rs)
(set-nonterminals! nt) 
(set-terminals! tr)  
(set-axiom! 'S)


To define the precedence matrix, we use the procedure compute-precs:

(define pr 
    (compute-precs rs nt tr 'S))


Then we are able to compute the transition function delta of the automaton:

(define dl (compute-delta))

and its inital states:

(define init (compute-initial-states))


Last and as before, we define the actual automaton for the interpreter:

(define automa 
  (create-automaton
    (the-states)   
    (the-terminals)
    (end-states 'S)
    pr
    dl
    #t))


We are now able to test it on a string:

(run-it automa '(a x a + a) init)




--- FREE/MAX-GRAMMARS: EXAMPLES AND USAGE (2) ---

The precedence matrix is given through the functions called def=., def<., def>.
Each function (say, of x) must be a case that returns the set of symbols that
are in that relation with x.

For instance:
(def=. (lambda (x)
           (case x
             ('b (set 'a))
             (else (set)))))

(def<. (lambda (x)
         (case x
           ('b (set 'b))
           (else (set)))))

stand for b =. a, b <. b


- "compute-chains" computes all the chains for a given input alphabet.

- "chains->grammar"  gets chains as first input, and a filtering function F as
  second input, and returns the corresponding free grammar. F must be
  a predicate on nonterminals, and is used to discard all the found nonterminals
  that do not comply with it. 

- If needed, there is also the function called "grammar-strings", that given
  a grammar G and a bound k, generates within the bound strings of G. 
  (The bound is checked on generated sentential forms, so it may generate strings
  longer than k, in some cases.)

To run the examples, e.g.: 
racket examples-free/matr1.ss

