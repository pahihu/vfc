( https://www.forth.com/embedded/#Annotated_example_source_code )

( Washing Machine Embedded Application )
\ Port assignments
01 CONSTANT PORT

\ bit-mask     name      bit-mask    name
    1 CONSTANT MOTOR      8 CONSTANT FAUCETS
    2 CONSTANT CLUTCH    16 CONSTANT DETERGENT
    4 CONSTANT PUMP      32 CONSTANT LEVEL

\ Device control
: ON ( mask -- ) PORT C@ OR PORT C! ;
: OFF ( mask -- ) INVERT PORT C@ AND PORT C! ;

\ Timing functions
: SECONDS ( n -- ) 0 ?DO 1000 MS LOOP ;
: MINUTES ( n -- ) 60 * SECONDS ;

: TILL-FULL ( -- ) \ Wait till level switch is on
     BEGIN PORT C@ LEVEL AND UNTIL ;

\ Washing machine functions
: ADD ( port -- ) DUP ON 10 SECONDS OFF ;
: DRAIN ( -- ) PUMP ON 3 MINUTES ;
: AGITATE ( -- ) MOTOR ON 10 MINUTES MOTOR OFF ;
: SPIN ( -- ) CLUTCH ON MOTOR ON
    5 MINUTES MOTOR OFF CLUTCH OFF PUMP OFF ;
: FILL-TUB ( -- ) FAUCETS ON TILL-FULL FAUCETS OFF ;

\ Wash cycles
: WASH ( -- ) FILL-TUB DETERGENT ADD AGITATE DRAIN ;
: RINSE ( -- ) FILL-TUB AGITATE DRAIN ;

\ Top-level control
: WASHER ( -- ) WASH SPIN RINSE SPIN ;



