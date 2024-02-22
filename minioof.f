( Mini OOF -- Bernd Paysan)
: method ( m v-m' v) create  over , swap cell+ swap
  does> ( ... o-...) @ over @ + @ execute ;
: var ( m v size-m v') create  over , +
  does> ( o -- addr) @ + ;
: class ( class-class methods vars) dup 2@ ;
: /end ( bytes)
  cell / 2 - dup 0 > IF
    for ['] noop , next
  ELSE drop
  THEN ;
: end-class  ( class methods vars)
  create  here >r , dup ,
  /end
  cell+ dup cell+ r> rot @ 2 cells /string move ;
: defines ( xt class) ' >body @ + ! ;
: new ( class-o)  here over @ allot swap over ! ;
: :: ( class "name") ' >body @ + @ compile, ;
create object  1 cells , 2 cells ,
