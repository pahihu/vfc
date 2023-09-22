: fib ( n-m)   dup 2 < if exit then 1- dup fib swap 1- fib + ;
: inner ( n)   for next ;
: outer ( n)   dup for  dup inner  next drop ;

20000 constant 20k
: mentink
   cr ." Mentink 20K "
   counter  20k 1- outer  timer ;

: benchFib
   cr ." Fibonacci(35) "
   counter  35 fib drop  timer ;

500000000 constant 500m
: bench1
   cr ." 500M loop FOR NEXT "
   counter  500m 1- for next  timer ;

: bench2
   cr ." 500M loop BEGIN UNTIL "
   counter
      500m begin
         1-
         dup 0=
      until  drop timer ;

: run
   cr ." Running benchmarks..."
   bench1
   bench2
   benchFib
   mentink
   cr ." Done " ;

run
