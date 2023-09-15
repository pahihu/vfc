( Round-robin multitasking)
variable tasking

2 cells constant 2cells
3 cells constant 3cells
: .status ( ta - a)   cell+ ;
: .s ( ta - a)   2cells + ;
: .s0 ( ta - a)   3cells + ;

: .task ( ta)
   cr ."   task " dup   .h
   cr ."   link " dup @ .h
   cr ." status " dup .status @ .h
   cr ."      s " dup .s      @
   dup .h
   if   cr ."      r " dup .s  @   @ .h
        cr ."     xt "     .s  @ @ @ .h
   else drop
   then ;

: rr   R> drop  tasking @  @  dup tasking !  .status @ >R ;

: asleep   rr ;
' asleep cell+ constant 'asleep
: sleep ( ta-ta)   'asleep  over .status ! ;
: awake  tasking @  sleep  .s @  sp! rp! r> drop ;

' awake cell+ constant 'awake
: wake ( ta-ta)   'awake over .status ! ;
: stop   0 >R rp@  sp@  tasking @ .s !  rr ;
: pause  tasking @ wake drop  stop ;

256 constant |S|
256 constant |R|

: alloc ( n-a)   cells allot here ;
: task ( #s #r 'name' - ta)
   alloc cell- >R  alloc cell- R> ( 's 'r)  over !
   create ( link) 0 , ( status) 0 , ( s s0) dup , , ;

: construct ( ta)   sleep
   tasking @ @  over !  dup tasking @ !  tasking ! ;
: /task ( ta)   dup .s0 @  swap .s ! ;
: activate ( ta)
   dup /task  wake  .s @ @ ( rp) R> swap !  ( rr) ;
: halt ( ta)   activate stop ;

create operator  operator , 'asleep , 0 ,
operator tasking !
