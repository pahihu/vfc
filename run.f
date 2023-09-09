decimal
32 constant bl


( Arithmetic ------------------------------------------------- )
: cell/ ( n1 - n2)   cell / ;
: cell- ( n1 - n2)   cell - ;


( Stack ------------------------------------------------------ )
: rot ( a b c -- b c a)   >R swap R> swap ;
: nip ( a b - b)   swap drop ;


( Memory ----------------------------------------------------- )
: @+ ( a1 - a2 x)   >a a@+ a> swap ;
: !+ ( a1 x - a2)   >a a!+ a> ;


( Control structures ----------------------------------------- )
: @xecute ( a)   @ if execute then ;


( Double words ----------------------------------------------- )
: 2! ( lo hi a)   >a a!+ a! ;
: 2@ ( a - lo hi)   >a a@+ a@ swap ;

: 2dup ( a b - a b a b)   over over ;
: 2drop ( a b)   drop drop ;
: 2swap ( a b c d - c d a b)   rot >R rot R> ;
: 2over ( a b c d - a b c d a b)   >R >R 2dup R> R> 2swap ;
: 2rot ( a b c d e f - c d e f a b)   >R >R 2swap R> R> 2swap ;

: 2>r ( a b)   R> rot >R swap >R >R ;
: 2r> ( - a b)   R> R> R> swap rot >R ;


: char ( 'name' - c)   bl word count  drop c@ ;

: >body ( xt - a)   [ 2 cells ] literal + ;

macro
: [char]   char postpone literal ;
: ['] ( 'name')   ' postpone literal ;
forth


( Debug ------------------------------------------------------ )
: .h  hex . decimal ;
: .line ( a - a')   7 for dup c@ 255 and space 3 .r 1+ next ;
: dump ( a n)
   15 + 16 / for
      cr  dup 8 .r  .line 3 spaces .line
   next  drop ;
: ? ( a)   @ . ;


( Values ----------------------------------------------------- )
variable #msg  0 #msg !

create values  ' @  , ' !  , ' +! ,

: value ( n 'name')
   create ,
   does>  values #msg @ cells +  @ execute  0 #msg ! ;

: to  1 #msg ! ;   : +to  2 #msg ! ;


( Strings ---------------------------------------------------- )
: pad ( - ca)   here [ cell 16 * ] literal + ;
: >pad ( ca1 u1 - ca2)   pad place  pad ;

create $pad 256 allot
: c( ( 'text' - a)   41 word count $pad place $pad ;
: s( ( 'text' - ca u)   c( count ;
: .( ( 'text')   s( type ;
: s+ ( to ca u - to)   >R over R> swap append ;

: (abort) ( n ca)   swap IF  count type abort  THEN  drop ;


macro
: s" ( 'text')   postpone c"  postpone count ;
: abort" ( 'text')   postpone c" postpone (abort) ;
forth

( Structures ------------------------------------------------- )
: +field ( o1 n 'name' - o2)   create over , + does> @ + ;


( Deferred words --------------------------------------------- )
: noop ;

: defer ( 'name')   create ['] noop , does>  @ execute ;
: is ( xt 'name')   ' >body ! ;
macro
: [is] ( 'name')   ' >body postpone literal postpone ! ;
forth


( Foreign interface ------------------------------------------ )
: library ( 'name')   bl word (dlopen) ;
: function: ( narg 'name')
   here >r
   create , r> (dlsym) dup 0= abort" undefined" ,
   does>  >a a@+ a@ (callc) ;


( Timer ------------------------------------------------------ )
2 function: gettimeofday

: (get-time) ( - usec sec)   0 0 sp@ 0 swap gettimeofday drop ;

variable diff0
: counter ( - msec)   (get-time) 1000 * swap 1000 / + ;
: timer ( n1)   counter swap - dup diff0 ! . ;
