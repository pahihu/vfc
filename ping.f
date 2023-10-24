( rr-multitasker example)
|s| |r| task t1  t1 build
|s| |r| task t2  t2 build

channel c1

: string ( # 'name')   create 0 , allot ;
: /string ( s)   0 swap ! ;

cell string buf1
cell string buf2

1000000 constant 1M

: run1
   t1 activate
   0 sp@ cell buf1 append
   buf1 .buf
   begin
      buf1 c1 !!
      buf1 /string
      buf1 cell c1 ??
   buf1 cell+ @
   1M > until  bye ;

: run2
   t2 activate
   begin
      buf2 /string
      buf2 cell c1 ??
      cr ." world" buf2 cell+ ?
      1 buf2 cell+ +!
      buf2 c1 !!
   0 until ;

: run   run1 run2 rr ;

run
