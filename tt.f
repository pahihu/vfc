|s| |r| task t1  t1 construct
|s| |r| task t2  t2 construct

variable round  0 round !
: run1   t1 activate
   begin
      1 round +!
      cr round @ . ." hello" pause
   0 until ;
: run2   t2 activate begin     ." world" pause  0 until ;

: run   run1 run2 rr ;