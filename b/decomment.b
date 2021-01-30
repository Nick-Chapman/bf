
Decomment a bf program: (Towards a self interpreter)

>>>			    leave a zero at the start

read in everything coding the 9 special chars

     hex;dec;diff;code
bang  21; 33;	; 9
plus  2b; 43; 10; 8
comma 2c; 44; 1;  7
minus 2d; 45; 1;  6
dot   2e; 46; 1;  5
left  3c; 60; 14; 4
right 3e; 62; 2;  3
open  5b; 91; 29; 2
close 5d; 93; 2;  1


,[			    read loop

    >>+<<		    flag

    --->+++++[-<------>]<[  reduce 33
    ->+++[-<--->]<[	    reduce 10
    -[			    reduce 1
    -[			    reduce 1
    -[			    reduce 1
    +>+++[-<----->]<[	    reduce 14
    --[			    reduce 2
    +>+++++[-<------>]<[    reduce 29
    --[			    reduce 2

    >>-<< [-]		    unflag and terminate switch

    ] >>[-<+>]<<
    ] >>[-<++>]<<
    ] >>[-<+++>]<<
    ] >>[-<++++>]<<
    ] >>[-<+++++>]<<
    ] >>[-<++++++>]<<
    ] >>[-<+++++++>]<<
    ] >>[-<++++++++>]<<
    ] >>[-<+++++++++>]<<

    >[-<+>]<		    move code leftwards
    [>]			    step if we coded
,]


<[<]>			    back to start

[
    <++++++++++>
    [-<-<+>>]		subtract code from 10
    <

    decode and print correct bf operator char

      >+++++++++++++++++++++++++++++++++<	33
    -[>++++++++++<				10
    -[>+<					 1
    -[>+<					 1
    -[>+<					 1
    -[>++++++++++++++<				14
    -[>++<					 2
    -[>+++++++++++++++++++++++++++++<		29
    -[>++<					 2
    [-]]]]]]]]]

    >.[-]<	print decoded
    
    >>
]
++++++++++.[-]		    print newline


shift codes back again
<<<
[ [->+<] < ]

