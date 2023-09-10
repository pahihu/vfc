( C.H.Moore: "FORTH: A new way to program a mini-computer" )

( Pass one number from one device to the other )
: PASS READ SEND ;

( To avoid sending negative numbers, clip negative numbers at 0 )
: POSITIVE READ 0 MAX SEND ;

( Pass 32 items )
: MANY 32 0 DO PASS LOOP ;

( Pass a variable number of items )
: TIMES ( n) 0 DO PASS LOOP ;

( Read the numbers and save them on disk )
100 INTEGER DATA

: STORE ( n sample#) DATA @ BLOCK + ! UPDATE ;

: SAMPLE ( sample#) READ SWAP STORE ;

( Save 32 samples on the disk )
: RECORD 32 0 DO I SAMPLE LOOP ;

( Do this every 10 seconds for an hour )
: MONITOR 600 0 DO RECORD 1 DATA + ! 10000 DELAY LOOP ;

