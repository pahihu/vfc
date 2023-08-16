# VFC

What is needed to write apps in Forth ?


C.H.Ting: "Forth Virtual Computer" contains 38 words.

    memory      C@ C! @ !
    arithmetic  + - * / MOD
    logic       AND OR XOR INVERT
    stack       DROP DUP SWAP OVER >R R@ R>
    i/o         IN OUT
    control     IF ELSE THEN  BEGIN WHILE REPEAT AGAIN  DO LOOP ' EXECUTE
                EXIT
    defining    CONSTANT VARIABLE : ;


C.H.Moore: "Introductory vocabulary" contains 45 words, it has the same 
power as BASIC.

    + - * / MOD MIN MAX
    < > =
    AND OR XOR
    NEGATE ABS NOT
    */
    DUP DROP SWAP OVER
    DECIMAL HEX OCTAL
    . .R EMIT CR KEY
    : ... ;
    VARIABLE CREATE ALLOT , @ !
    ( ... )
    EMPTY
    IF ELSE THEN  FOR I NEXT

There are other words necessary to implement the interpreter, compiler,
block storage and console interaction. But for most apps the above
is enough.


G.Haydon: "Level 0 Forth", contains 37 words (C.H.Moore)

    + - * */ /MOD ABS NEGATE MIN MAX
    AND OR XOR NOT
    DROP DUP SWAP OVER
    DECIMAL HEX OCTAL . n .R
    CR EMIT KEY
    : ; CREATE , ALLOT
    IF ELSE THEN FOR NEXT I

Foreign function interface example

    ===> cat test.c
    #include <stdio.h>
    void test_c(int n) { printf("n = %d\n", n); }

    ===> cc -shared -fPIC -o libtest.so test.c

    ===> vfc run.f
    library ./libtest.so

    0 0 0 0 > 1 function: test_c

    0 0 0 0 > 42 test_c drop
    n = 42

    0 0 0 0 > bye
