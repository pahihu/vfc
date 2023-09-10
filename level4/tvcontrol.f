( microFORTH PRIMER: TV Remote Control pp. 52 )

0 CVARIABLE PADDLE

: READ   PADDLE  C@ ;

: INPUT   BEGIN  READ  -DUP  END ;

( Stubs only ------------------------------------------------- )

: DIGITAL   0  0 PADDLE C! ;
: ANALOG   1  0 PADDLE C! ;

: PROCESS   DUP 13 > IF ANALOG  ELSE DIGITAL  THEN ;

: IDLE   BEGIN  READ  NOT  END ;

: TV   BEGIN INPUT  PROCESS  IDLE  0 END ;
