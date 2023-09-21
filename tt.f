( rr-multitasker example)
|s| |r| task t1  t1 build
|s| |r| task t2  t2 build

: off ( a)   0 swap ! ;

: run1
   t1 sender 's off  t1 activate
   0
   begin
      cr dup . ." hello"
      t2 send  receive drop
   0 until ;

: run2
   t2 sender 's off  t2 activate
   begin
      receive ( msg from) >R
      ." world" pause
      1+ R> send
   0 until ;

: run   run1 run2 rr ;
