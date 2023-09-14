decimal
32 constant bl


( Arithmetic ------------------------------------------------- )
: cell/ ( n1 - n2)   cell / ;
: cell- ( n1 - n2)   cell - ;


( Stack ------------------------------------------------------ )
: rot ( a b c -- b c a)   >R swap R> swap ;
: nip ( a b - b)   swap drop ;


( Memory ----------------------------------------------------- )
: @+ ( a1 - a2 x)   dup cell+ swap @ ;
: !+ ( a1 x - a2)   over ! cell+ ;


( Control structures ----------------------------------------- )
: @execute ( a)   @ dup if drop execute exit then drop ;


( Compiler --------------------------------------------------- )
: compile   R> dup @ , cell+ >R ;
: lit ( x)   compile <lit> , ;
macro
: \ ( 'name')   ' , ;
: aft ( a - a A)   drop compile branch here 0 , here swap ;
forth



( Double words ----------------------------------------------- )
: 2! ( lo hi a)   swap over ! cell+ ! ;
: 2@ ( a - lo hi)   dup cell+ @ swap @ ;

: 2dup ( a b - a b a b)   over over ;
: 2drop ( a b)   drop drop ;
: 2swap ( a b c d - c d a b)   rot >R rot R> ;
: 2over ( a b c d - a b c d a b)   >R >R 2dup R> R> 2swap ;
: 2rot ( a b c d e f - c d e f a b)   >R >R 2swap R> R> 2swap ;

: 2>r ( a b)   R> rot >R swap >R >R ;
: 2r> ( - a b)   R> R> R> swap rot >R ;


: char ( 'name' - c)   bl word count  drop c@ ;

2 cells constant +body
: >body ( xt - a)   +body + ;

macro
: [char]   char lit ;
: ['] ( 'name')   ' lit ;
forth


( Debug ------------------------------------------------------ )
: .h  base @ >R hex . R> base ! ;
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
: >pad ( ca1 u1 - ca2)   pad place  pad ;
: print ( ca)   count type ;

create $pad 256 allot
: c( ( 'text' - a)   [char] ) word count $pad place $pad ;
: s( ( 'text' - ca u)   c( count ;
: .( ( 'text')   s( type ;
: s+ ( to ca u - to)   >R over R> swap append ;
: ," ( 'text")   [char] " word count here swap  dup allot  move ;

: (abort) ( n ca)   swap IF  print abort  THEN  drop ;


macro
: ." ( 'text')   \ c" compile print ;
: s" ( 'text')   \ c"  compile count ;
: abort" ( 'text')   \ c" compile (abort) ;
forth

( Structures ------------------------------------------------- )
: +field ( o1 n 'name' - o2)   create over , + does> @ + ;


( Deferred words --------------------------------------------- )
: noop ;

: defer ( 'name')   create ['] noop , does>  @ execute ;
: is ( xt 'name')   ' >body ! ;
macro
: [is] ( 'name')   ' >body lit compile ! ;
forth


( Coroutines ------------------------------------------------- )
: co   R> R> swap >R >R ;


( Foreign interface ------------------------------------------ )
: library ( 'name')   bl word (dlopen) ;
: function: ( narg 'name')
   here >r
   create , r> (dlsym) dup 0= abort" undefined" ,
   does>  dup @ swap cell+ @ (callc) ;


( Timer ------------------------------------------------------ )
2 function: gettimeofday

: (get-time) ( - usec sec)   0 0 sp@ 0 swap gettimeofday drop ;

variable diff0
: counter ( - msec)   (get-time) 1000 * swap 1000 / + ;
: timer ( n1)   counter swap - dup diff0 ! . ;
