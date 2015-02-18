#! /bin/bash

racket --script  examples/ex2+.scm  > out.txt 
racket --script  examples/ex3--.scm  >> out.txt
racket --script  examples/ex3+.scm  >> out.txt
racket --script  examples/ex4-.scm  >> out.txt
racket --script  examples/ex4.scm  >> out.txt
racket --script  examples/ex5.scm  >> out.txt
racket --script  examples/ex6.scm  >> out.txt
racket --script  examples/ex_gr_dino.scm >> out.txt
