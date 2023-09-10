( FORTH, Inc. SwiftX-LED-Demo.zip )

0 [IF]

Speed of Morse code is specified in words per minute (WPM).

   dah = 3 * dit
 
Spacing
- between dits and dahs within a character is 1 dit
- between letters in a word is a dah (3 dits)
- between words it is 7 dits

Paris standard defines the speed of Morse transmission as the dot and
dash timing needed to send the word "Paris" a given number of times
per minute. "Paris" is precisely 50 dits.

Unit for 1 dit can be computed by

        Tu = 1200 / W

Where: W is the desired speed in words-per-minute, and Tu is one
dit-time in milliseconds.

When Tu is 120, it sets the initial rate at 10 WPM.

[THEN]


( Timing ----------------------------------------------------- )

CREATE Tu 120 ,

: WPM ( n -- )   1200 SWAP / Tu ! ;
: DELAY ( n -- )   Tu @ * MS ;


( Morse code elements ---------------------------------------- )

: DIT ( -- )   +LED  1 DELAY  -LED  1 DELAY ;
: DAH ( -- )   +LED  3 DELAY  -LED  1 DELAY ;


( Character codes -------------------------------------------- )

: S ( -- )   DIT DIT DIT  2 DELAY ;
: O ( -- )   DAH DAH DAH  2 DELAY ;


( Distress signal -------------------------------------------- )

: SOS ( -- )   S O S  4 DELAY ;


( Background task -------------------------------------------- )

|U| |S| |R| BACKGROUND BEACON

: /BEACON ( -- )   /LED
   BEACON ACTIVATE  BEGIN  SOS  AGAIN ;

