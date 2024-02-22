: /string ( ca # n-ca+n #-n)   swap over - 0 max >R + R> ;
: compile, ( xt)   , ;

( System stack ----------------------------------------------- )
create SyST 16 cells allot
variable 'SyP  SyST 16 cells + 'SyP !
: >s ( x)   cell negate 'SyP +!  'SyP @ ! ;
: s> ( -x)   'SyP @ @  cell 'SyP +! ;

( Facility --------------------------------------------------- )
: csi   27 emit [char] [ emit ;
: .csi ( n)   (.) type ;
: cup ( row col)
   csi .csi [char] ; emit
       .csi [char] H emit ;
: cls   csi 2 .csi [char] J emit ;
: at-xy ( col row)   1+ swap 1+ swap cup ;
: home   0 0 at-xy ;
: page   home cls ;

( vim:set ts=3 sw=3 et:)
