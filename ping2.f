( rr-multitasker example)
|s| |r| task t1  t1 build
|s| |r| task t2  t2 build

1m constant 1m

channel c1

: run1
   t1 activate
   0
   begin
      c1 cell!!
      c1 cell??
   dup 1m > until  bye ;

: run2
   t2 activate
   begin
      c1 cell??
      cr ." world" dup .
      1+ c1 cell!!
   0 until ;

: run   run1 run2 rr ;

run
