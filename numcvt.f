( Number conversion ------------------------------------------ )

variable hld
create digits ," 0123456789abcdefghijklmnopqrstuvwxyz"
: hldbuf ( -a)   pad 80 + ;
: <#   hldbuf hld ! ;
: hold ( c)   -1 hld +!  hld @ c! ;
: # ( n1-n2)   base @ /mod swap  digits + c@  hold ;
: sign ( n)   0 < if [char] - hold then ;
: #s ( n)   begin # dup 0= until ;
: #> ( x-a #)   drop hld @ dup  hldbuf swap - ;
: (.) ( n-a #)   dup abs <# #s swap sign #> ;

( vim:set ts=3 sw=3 et:)
