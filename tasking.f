( round-robin multitasking)
variable tasking

2 cells constant 2cells
3 cells constant 3cells
: tid ( -tid)   tasking @ ;                 ( current task addr)
: follower ( -a)   tid @ ;             ( addr of following task)
: status (  -a)   tid cell+ ;            ( contains 'wake/'pass)
: s ( -a)   tid 2cells + ;                 ( contains stack ptr)
: s0 ( -a)   tid 3cells + ;        ( contains initial stack ptr)

: 's ( 'tid a - a)   tid - + ;  ( access other task's variables)

: .task ( ta)                                       ( dump task)
   cr ."   task " dup   .h
   cr ."   link " dup @ .h
   cr ." status " dup status 's @ .h
   cr ."      s " dup s      's @
   dup .h
   if   cr ."      r " dup s 's  @   @ .h
        cr ."     xt "     s 's  @ @ @ .h
   else drop
   then ;

( round-robin scheduler----------------------------------------)
( get next tasks address, makes current and jumps to the addr  )
( in status)
: rr   R> drop  follower  tasking !  status @ >R ;

: pass   rr ;                                      ( do nothing)
' pass cell+ constant 'pass         ( addr of pass' thread list)
: sleep ( tid)   status 's  'pass swap !  ;    ( put task sleep)
: wake ( - )   tid sleep  s @  sp! rp! r> drop ;    ( wake task)

' wake cell+ constant 'wake         ( addr of wake' thread list)
: awake ( tid)   status 's  'wake swap ! ;      ( awaken a task)
: stop ( - )   0 >R rp@  sp@  s !  rr ;             ( stop task)
: pause ( - )   tid awake  stop ;               ( switch tasks)

256 constant |S|                      ( default data stack size)
256 constant |R|                    ( default return stack size)

: alloc ( n-a)   cells allot here ;   ( allot stack, grows down)
: task ( #s #r 'name' - tid)                      ( create task)
   alloc cell- >R  alloc cell- R> ( 's 'r)  over !
   create ( link) 0 , ( status) 0 , ( s s0) dup , , ;

: build ( tid)   dup sleep                          ( init task)
   follower  over !  dup tid !  tasking ! ;
: /task ( tid)   dup s0 's @  swap s 's ! ;     ( reset s to s0)
: activate ( tid)                                  ( start task)
   dup /task  dup awake  s 's @ @ ( rp) R> swap !  ( rr) ;
: halt ( tid)   activate stop ;                     ( halt task) 

( initial task: operator)
create operator  operator , 'pass , 0 ,
operator tasking !


( FORTH, Inc. like facility variables)
: free ( a - a t)   @ dup 0=  swap                   ( either 0)
   status =  or ;                    ( or owned by current task)
: get ( a)   begin  pause  free until         ( wait until free)
   status swap ! ;                ( store current task's status)
: release ( a)   free  if  0 swap !  else
   drop  then ;

( B.Rodriguez multitasking)
: wait ( a)   begin  pause  dup @  until   ( wait for available)
   0 swap ! ;
: signal ( a)   1 swap ! ;                  ( make it available)

variable sender  variable message    ( should be user variables)
: send ( msg to)
   tid  over sender 's                   ( msg to me SENDERaddr)
   begin  pause  dup @ 0=  until
   !  message 's ! ;

: receive ( -- msg from)
   begin  pause sender @  until    ( wait for a msg from anyone)
   message @  sender @                ( get msg and sender task)
   0 sender ! ;                         ( ready for another msg)
