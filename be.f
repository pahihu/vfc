( block editor)
variable scr
: ba ( -a)   scr @ block ;
: .head   cr ." scr#" scr @ . ;
: .line ( a i-a')   15 swap -  cr 2 .r space  dup 64 type 64 + ;
: l .head ba 15 for i .line next drop cr ;
: line ( n - a)   64 *  scr @ block  + ;
: list ( n)   scr ! l ;
: wipe   ba 1024 bl fill ;
: e ( n)   line 64 bl fill ;
: r ( n 'text')   dup e line >R  13 word count  R> swap move ;
: go ( o)   scr +! l ;  : p   -1 go ;  : n    1 go ;
: insdel ( n - a b #)   15 over - 64 * >R  line dup 64 +  R> ;
: d ( n)   insdel >R swap R> move  15 e ;
: ins ( n)   dup  insdel move  e ;
: a ( n 'text')   dup ins   r ;  : x   scr @ load ;

: h
   cr ." LIST ( n)      list Nth block"
   cr ." WIPE           blank block"
   cr ." L              list block"
   cr ." D ( n)         delete Nth line"
   cr ." E ( n)         clear Nth line"
   cr ." R ( n 'txt')   replace Nth line with txt"
   cr ." A ( n 'txt')   add (insert) Nth line with txt"
   cr ." X              evaluate block"
   cr ." P              move to previous block"
   cr ." N              move to next block" ;
