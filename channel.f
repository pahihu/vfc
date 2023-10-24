#user @
cell +user 'chan        ( actual chanptr)
cell +user chan.buf     ( counted string)
cell +user chan.len     ( expected input length)
#user !

: .buf ( s)   dup .h count dup .
   1- for dup i + c@ .h next  drop ;

: other.tid ( - a)   'chan @ @ ;
: other.buf ( - s)   other.tid chan.buf 's @ ;
: other.len ( - a)   other.tid chan.len 's   ;
: /channel ( s # chan) 'chan !  negate chan.len !  chan.buf ! ;

: channel ( 'name')   create  0 , ;

: ?chan ( -t)   other.tid ;
: await   tid 'chan @ !  stop ;
: done? ( -t)   other.len @ 0= ;
: reschedule   other.tid awake  0 'chan @ ! ;

: msg.append ( a1 n1 s2 a)   >R over R> +!  append ;
: msg.send   chan.buf @ count other.buf  other.len msg.append ;
: msg.recv   other.buf  count chan.buf @ chan.len  msg.append ;

: !! ( s chan)
   0 swap /channel
   ?chan if   msg.send done?  if  reschedule  then
         else await
         then ;

: ?? ( s # chan)
     /channel
     ?chan if  msg.recv  reschedule  then
chan.len @ if            await       then ;

: byte!! ( c chan)   >R          1 sp@       R> !!  drop drop ;
: cell!! ( x chan)   >R       cell sp@       R> !!  drop drop ;
: byte?? ( chan-c)   >R    0     0 sp@     1 R> ??  drop ;
: cell?? ( chan-x)   >R    0     0 sp@  cell R> ??  drop ;
