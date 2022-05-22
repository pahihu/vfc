32 constant bl
cell 8 * constant bits

: words ( #b - #w)   [ cell 1- ] literal +  cell/ ;

( Double words ----------------------------------------------- )
: 2! ( lo hi a)   swap over ! cell+ ! ;
: 2@ ( a - lo hi)   dup @ >R cell+ @ R> ;

: 2dup ( a b - a b a b)   over over ;
: 2drop ( a b)   drop drop ;
: 2swap ( a b c d - c d a b)   rot >R rot R> ;
: 2over ( a b c d - a b c d a b)   >R >R 2dup R> R> 2swap ;
: 2rot ( a b c d e f - c d e f a b)   >R >R 2swap R> R> 2swap ;

: 2>r ( a b)   R> rot >R swap >R >R ;
: 2r> ( - a b)   R> R> R> swap rot >R ;


: char ( 'name' - c)   bl word count  drop c@ ;

: >body ( xt - a)   [ 2 cells ] literal + ;
: compile, ( xt)   , ;

' <lit> constant doLIT
: lit, ( n)   doLIT , , ;

macro
: [char]   char lit, ;
forth


( Debug ------------------------------------------------------ )
: .h  hex . decimal ;
: .line ( a - a')   8 for dup c@ 255 and space 2 .r 1+ next ;
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
   does>  values #msg @ cells +  @execute  0 #msg ! ;

: to  1 #msg ! ;   : +to  2 #msg ! ;


( Strings ---------------------------------------------------- )
: pad ( - ca)   here bits 2 * + ;
: >pad ( ca1 u1 - ca2)   pad place  pad ;

create $pad 256 allot
: s( ( 'text' - ca u)
   [char] ) word count  $pad place  $pad count ;
: s+ ( to ca u - to)   rot dup >R append R> ;

: (abort) ( n ca)   swap IF  count type abort  THEN  drop ;

macro
: s" ( 'text')   postpone c"  postpone count ;
: abort" ( 'text')   postpone c" postpone (abort) ;
forth


( Deferred words --------------------------------------------- )
: noop ;

: defer ( 'name')   create ['] noop , does>  @execute ;
: is ( xt 'name')   ' >body ! ;
macro
: [is] ( 'name')   ' >body lit, postpone ! ;
forth


( Foreign interface ------------------------------------------ )
: library ( 'name')   bl word (dlopen) ;
: function: ( narg 'name')
   here >r
   create , r> (dlsym) dup 0= abort" undefined" ,
   does>  @+ swap @ (callc) ;


( Timer ------------------------------------------------------ )
2 function: gettimeofday

: (get-time) ( - usec sec)   0 0 sp@ 0 swap gettimeofday drop ;

variable diff0
: counter ( - msec)   (get-time) 1000 * swap 1000 / + ;
: timer ( n1)   counter swap - dup diff0 ! . ;
