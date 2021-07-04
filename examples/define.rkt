#lang forth

: DOUBLE 2 * ;

10
DOUBLE
DOUBLE

: fac ( n -- n! ) 1 swap 1+ 1 ?do i * loop ;

5
fac
